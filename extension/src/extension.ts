import axios from 'axios';
import * as vscode from 'vscode';
import path = require('path/posix');
import fs = require('node:fs');
import { ChildProcess, exec, spawn } from 'child_process';
import { homedir } from 'os'
import { SingleEntryPlugin } from 'webpack';


const userHomeDir = homedir();
const serverRoot = path.join(userHomeDir, "/.local/share/haskell-gtd-extension-server-root");
const serverRepos = path.join(serverRoot, "repos");
const serverExe = "haskell-gtd-server";
const serverPidF = path.join(serverRoot, 'pid');
const serverPortF = path.join(serverRoot, 'port');

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
		console.log('symlink %s -> %s failed: %s', src, dst, error);
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
			console.log(error);
			return null;
		});
	console.log("heartbeat: %s", res);
	return res != null;
}

async function startServerIfRequired() {
	if (await sendHeartbeat()) return;
	console.log("starting server");

	if (stdout != null) { fs.closeSync(stdout); stdout = null; }
	if (stderr != null) { fs.closeSync(stderr); stderr = null; }
	if (server != null) { server.kill(); server = null; }

	stdout = fs.openSync(path.join(serverRoot, 'stdout.log'), 'a');
	stderr = fs.openSync(path.join(serverRoot, 'stderr.log'), 'a');
	server = spawn(path.join(serverRoot, serverExe), {
		detached: true,
		stdio: ['ignore', stdout, stderr],
	});
	server.unref();
	await new Promise(r => setTimeout(r, 3000));
	await sendHeartbeat();
}

class XDefinitionProvider implements vscode.DefinitionProvider {
	async provideDefinition(
		document: vscode.TextDocument,
		position: vscode.Position,
		token: vscode.CancellationToken
	): Promise<vscode.Definition> {
		let range = document.getWordRangeAtPosition(position);
		let word = document.getText(range);

		console.log("%s", word);

		if (!vscode.workspace.workspaceFolders) {
			return Promise.resolve([]);
		}

		let workspaceFolder = vscode.workspace.workspaceFolders[0];
		let workspacePath = workspaceFolder.uri.fsPath;
		let docPath = document.uri.fsPath;

		if (!isParentOf(workspacePath, docPath)) {
			console.log("workspacePath is not parent of docPath, this case is broken right now");
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
				console.log("for body {body}");
				console.log(error);
				return { "data": {} };
			});
		let data = res.data;
		console.log(data);
		console.log(res);
		if (data.err != "" && data.err != undefined) {
			console.log("%s -> err:%s", word, data.err);
			return Promise.resolve([]);
		}

		let symlink = path.join(workspacePath, "./.repos");
		await createSymlink(serverRepos, symlink);

		let filePath = data.srcSpan.sourceSpanFileName;
		let filePathU;
		if (isParentOf(workspacePath, filePath)) {
			filePathU = filePath;
		} else if (isParentOf(serverRepos, filePath)) {
			filePathU = path.resolve(symlink, path.relative(serverRepos, filePath))
		} else {
			console.log("filePath is not parent of workspacePath or serverRepos");
			return Promise.resolve([]);
		}
		let fileUri = vscode.Uri.file(path.normalize(filePathU));

		let line = data.srcSpan.sourceSpanStartLine - 1; // 0-based line number
		let character = data.srcSpan.sourceSpanStartColumn - 1; // 0-based character position
		let definitionPosition = new vscode.Position(line, character);
		let definitionLocation = new vscode.Location(fileUri, definitionPosition);

		console.log("%s -> %s", filePath, filePathU);
		return Promise.resolve(definitionLocation);
	}
}

export function activate(context: vscode.ExtensionContext) {
	console.log('Congratulations, your extension "hs-gtd" is now active!');
	console.log(userHomeDir);


	context.subscriptions.push(
		vscode.workspace.onDidSaveTextDocument((document: vscode.TextDocument) => {
			let workspaceFolders = vscode.workspace.workspaceFolders;
			if (!workspaceFolders) {
				return;
			}
			let workspacePath = workspaceFolders[0].uri.fsPath;
			let symlink = path.join(workspacePath, "./.repos");

			console.log("saved languageId=%s: %s", document.languageId, document.uri.fsPath);
			if (isParentOf(symlink, document.uri.fsPath) || !(document.languageId == "haskell" || document.languageId == "cabal") || !isParentOf(workspacePath, document.uri.fsPath)) {
				return;
			}
			console.log("resetting workspace extension cache");
			fs.rmSync(path.join(workspacePath, "modules.json"), { recursive: false, force: true });
			fs.rmSync(path.join(workspacePath, "exports.json"), { recursive: false, force: true });
		})
	);

	context.subscriptions.push(
		vscode.languages.registerDefinitionProvider(
			{ scheme: 'file', language: 'haskell' },
			new XDefinitionProvider()
		)
	);

	let intervalId = setInterval(sendHeartbeat, 30 * 1000);
	context.subscriptions.push({ dispose: () => clearInterval(intervalId) });
}

// This method is called when your extension is deactivated
export async function deactivate() { }
