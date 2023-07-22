import axios from 'axios';
import * as vscode from 'vscode';
import path = require('path/posix');
import fs = require('node:fs');
import { ChildProcess, exec, spawn } from 'child_process';
import { homedir } from 'os'
import * as util from 'util';

const userHomeDir = homedir();
let serverRoot = path.join(userHomeDir, "/.local/share/haskell-gtd-extension-server-root");
let serverExe = path.join(serverRoot, "haskell-gtd-server");
let serverRepos = path.join(serverRoot, "repos");
let serverPidF = path.join(serverRoot, 'pid');
let serverPortF = path.join(serverRoot, 'port');
let outputChannel: vscode.OutputChannel;

function refreshSubPaths() {
	serverRepos = path.join(serverRoot, "repos");
	serverPidF = path.join(serverRoot, 'pid');
	serverPortF = path.join(serverRoot, 'port');
}

let port = 0;
let stdout: number | null = null, stderr: number | null = null;
let server: ChildProcess | null = null;

function isParentOf(p1: string, p2: string) {
	const relative = path.relative(p1, p2);
	return relative && !relative.startsWith('..') && !path.isAbsolute(relative);
}

async function createSymlink(src: string, dst: string) {
	try {
		await fs.promises.symlink(src, dst, 'file');
	} catch (error) {
		outputChannel.appendLine(util.format('symlink %s -> %s failed: %s', src, dst, error));
	}
}

async function sendHeartbeat() {
	fs.readFile(serverPortF, "utf8", (err, data) => {
		let pid = parseInt(data, 10);
		port = pid;
	});

	if (port <= 0) return false;

	let res = await axios.
		post(`http://localhost:${port}/ping`, null).
		catch(function (error) {
			outputChannel.appendLine(util.format(error));
			return null;
		});
	outputChannel.appendLine(util.format("heartbeat: %s", res));
	return res != null;
}

async function startServerIfRequired() {
	if (await sendHeartbeat()) return;
	outputChannel.appendLine(util.format("starting server"));

	if (stdout != null) { fs.closeSync(stdout); stdout = null; }
	if (stderr != null) { fs.closeSync(stderr); stderr = null; }
	if (server != null) { server.kill(); server = null; }

	let conf = vscode.workspace.getConfiguration('haskell-gtd');
	let as = conf.get<string[]>("server.args") ?? [];
	let rts = conf.get<string[]>("server.rts") ?? [];

	stdout = fs.openSync(path.join(serverRoot, 'stdout.log'), 'a');
	stderr = fs.openSync(path.join(serverRoot, 'stderr.log'), 'a');
	server = spawn(
		serverExe,
		[...as, "+RTS", ...rts, "-RTS"],
		{
			detached: true,
			stdio: ['ignore', stdout, stderr],
		}
	);
	server.unref();
	await new Promise(r => setTimeout(r, 3000));
	await sendHeartbeat();
}

async function stopServer() {
	let pid: number = -1;
	fs.readFile(serverPortF, "utf8", (err, data) => pid = parseInt(data, 10));
	if (pid <= 0) return;
	process.kill(pid);
}

class XDefinitionProvider implements vscode.DefinitionProvider {
	async provideDefinition(
		document: vscode.TextDocument,
		position: vscode.Position,
		token: vscode.CancellationToken
	): Promise<vscode.Definition> {
		let range = document.getWordRangeAtPosition(position);
		let word = document.getText(range);

		outputChannel.appendLine(util.format("%s", word));

		if (!vscode.workspace.workspaceFolders) {
			return Promise.resolve([]);
		}

		let workspaceFolder = vscode.workspace.workspaceFolders[0];
		let workspacePath = workspaceFolder.uri.fsPath;
		let docPath = document.uri.fsPath;

		if (!isParentOf(workspacePath, docPath)) {
			outputChannel.appendLine(util.format("workspacePath is not parent of docPath, this case is broken right now"));
			return Promise.resolve([]);
		}

		await startServerIfRequired();
		let body = {
			workDir: workspacePath,
			file: docPath,
			word: word
		};
		let res = await axios.
			post(`http://localhost:${port}/definition`, body).
			catch(function (error) {
				outputChannel.appendLine(util.format("for body {body}"));
				outputChannel.appendLine(util.format(error));
				return { "data": {} };
			});
		let data = res.data;
		if (data.err != "" && data.err != undefined) {
			outputChannel.appendLine(util.format("%s -> err=%s (data=%s)", word, data.err, JSON.stringify(data)));
			return Promise.resolve([]);
		}
		if (data.srcSpan == undefined) {
			outputChannel.appendLine(util.format("%s -> no srcSpan (data=%s)", word, JSON.stringify(data)));
			return Promise.resolve([]);
		}
		outputChannel.appendLine(util.format(data));

		let symlink = path.join(workspacePath, "./.repos");
		await createSymlink(serverRepos, symlink);

		let filePath = data.srcSpan.sourceSpanFileName;
		let filePathU;
		if (isParentOf(workspacePath, filePath)) {
			filePathU = filePath;
		} else if (isParentOf(serverRepos, filePath)) {
			filePathU = path.resolve(symlink, path.relative(serverRepos, filePath))
		} else {
			outputChannel.appendLine(util.format("filePath is not parent of workspacePath or serverRepos"));
			return Promise.resolve([]);
		}
		let fileUri = vscode.Uri.file(path.normalize(filePathU));

		let line = data.srcSpan.sourceSpanStartLine - 1; // 0-based line number
		let character = data.srcSpan.sourceSpanStartColumn - 1; // 0-based character position
		let definitionPosition = new vscode.Position(line, character);
		let definitionLocation = new vscode.Location(fileUri, definitionPosition);

		outputChannel.appendLine(util.format("%s -> %s", filePath, filePathU));
		return Promise.resolve(definitionLocation);
	}
}

export function activate(context: vscode.ExtensionContext) {
	outputChannel = vscode.window.createOutputChannel("haskell-gtd");
	
	outputChannel.appendLine(userHomeDir);

	context.subscriptions.push(
		vscode.workspace.onDidSaveTextDocument(async (document: vscode.TextDocument) => {
			let workspaceFolders = vscode.workspace.workspaceFolders;
			if (!workspaceFolders) {
				return;
			}
			let workspacePath = workspaceFolders[0].uri.fsPath;
			let symlink = path.join(workspacePath, "./.repos");

			outputChannel.appendLine(util.format("saved languageId=%s: %s", document.languageId, document.uri.fsPath));
			if (isParentOf(symlink, document.uri.fsPath) ||
				!(document.languageId == "haskell" || document.languageId == "cabal") ||
				!isParentOf(workspacePath, document.uri.fsPath)) return;
			outputChannel.appendLine(util.format("resetting workspace extension cache"));

			await startServerIfRequired();
			let body = {dir: workspacePath};
			let res = await axios.post(`http://localhost:${port}/dropcache`, body);
			outputChannel.appendLine(util.format("resetting workspace extension cache: %s", res.data));
		})
	);

	context.subscriptions.push(
		vscode.languages.registerDefinitionProvider(
			{ scheme: 'file', language: 'haskell' },
			new XDefinitionProvider()
		)
	);

	context.subscriptions.push(vscode.commands.registerCommand('hs-gtd.server.restart', stopServer));

	vscode.workspace.onDidChangeConfiguration((e) => {
		if (!e.affectsConfiguration('hs-gtd')) {
			return;
		}
		let conf = vscode.workspace.getConfiguration('hs-gtd');
		serverRoot = conf.get<string>('server.root') ?? path.join(userHomeDir, "/.local/share/haskell-gtd-extension-server-root");
		serverExe = conf.get<string>('server.path') ?? "hs-gtd-server";
		if (!path.isAbsolute(serverExe)) {
			serverExe = path.normalize(path.join(serverRoot, serverExe));
		}
		refreshSubPaths();

		if (e.affectsConfiguration("hs-gtd.server.rts") || e.affectsConfiguration("hs-gtd.server.args")) {
			stopServer();
		}
	});

	let intervalId = setInterval(sendHeartbeat, 30 * 1000);
	context.subscriptions.push({
		dispose: () => {
			clearInterval(intervalId);
			outputChannel.show();
		}
	});
}

export async function deactivate() { }
