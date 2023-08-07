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

export function insertAndShift(str: string, char: string, index: number) {
	return str.slice(0, index) + char + str.slice(index);
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
	let args = conf.get<string[]>("server.args") ?? [];
	let rts = conf.get<string[]>("server.rts") ?? [];

	stdout = fs.openSync(path.join(serverRoot, 'stdout.log'), 'a');
	stderr = fs.openSync(path.join(serverRoot, 'stderr.log'), 'a');
	server = spawn(
		serverExe,
		[...args, "+RTS", ...rts, "-RTS"],
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

// https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-140002
// `identifier` returns the longest identifier that contains the given position.
// Examples:
// `(.=)`, `.=` 			  => `.=`
// `(Prelude..)`, `Prelude..` => `Prelude..`
// `Prelude.flip`             => `Prelude.flip`
// `Data.List.map` 		  	  => `Data.List.map`
// `Data.List` 				  => `Data.List`
// `map` 					  => `map`
// `Data` 					  => `Data`
// `a.b` 					  => `b` (if the cursor is on `b`)
// `(a).(b)` 				  => `b` (if the cursor is on `b`)
// `A.a'b+c` 					  => `A.a'b` (if the cursor is on `A.a'b`)
//
// ```
/* This 	Lexes as this
	
f.g 	f . g (three tokens)e
F.g 	F.g (qualified ‘g’)
f.. 	f .. (two tokens)
F.. 	F.. (qualified ‘.’)
F. 		F . (two tokens) 
 */
const upPunctuation = /^\p{Punctuation}$/u;
const upLower = /^\p{Lowercase_Letter}$/u;
const upUpperOrTitle = /^\p{Uppercase_Letter}|\p{Titlecase_Letter}$/u;
const upDigit = /^\p{Number}$/u
export function identifier(s: string, pos0: number): [string, string[][]] {
	function isASCII(c: string) {
		let code = c.charCodeAt(0);
		return code >= 0 && code <= 127;
	}

	function isSymbol(c: string) {
		if (c.length != 1) throw new Error("isSymbol: `" + c + "`.length != 1");
		if (isASCII(c)) {
			// ! # $ % & ⋆ + . / < = > ? @ 	\ ^ - ~ : |
			// except `( ) , ; [ ] ` { }`
			return c == '!' || c == '#' || c == '$' || c == '%' || c == '&' || c == '*' || c == '+' || c == '.' || c == '/' || c == '<' || c == '=' || c == '>' || c == '?' || c == '@' || c == '\\' || c == '^' || c == '|' || c == '-' || c == '~' || c == ':';
		} else {
			return upPunctuation.test(c);
		}
	}

	function isSmall(c: string) {
		if (c.length != 1) throw new Error("isSmall: `" + c + "`.length != 1");
		return isASCII(c) ? c >= 'a' && c <= 'z' : upLower.test(c);
	}

	function isLarge(c: string) {
		if (c.length != 1) throw new Error("isSmall: `" + c + "`.length != 1");
		return isASCII(c) ? c >= 'A' && c <= 'Z' : upUpperOrTitle.test(c);
	}

	function isDigit(c: string) {
		if (c.length != 1) throw new Error("isSmall: `" + c + "`.length != 1");
		return isASCII(c) ? c >= '0' && c <= '9' : upDigit.test(c);
	}

	function isIdentChar(c: string) {
		return isSmall(c) || isLarge(c) || isDigit(c) || c == '\'';
	}

	enum Direction {
		Left,
		Right,
		Bidirectional,
	};

	// [left, right)
	function emitAt(pos: number, dir: Direction): [number, number, string] {
		let start = s[pos];
		if (start == null || start == undefined) return [-1, -1, ""];

		let p;
		if (isIdentChar(start)) {
			p = isIdentChar;
		} else if (isSymbol(start)) {
			p = isSymbol;
		} else {
			return [-1, -1, ""];
		}

		let left = pos;
		if (dir == Direction.Left || dir == Direction.Bidirectional) {
			while (left > 0 && p(s[left - 1])) left--;
		}

		let right = pos + 1;
		if (dir == Direction.Right || dir == Direction.Bidirectional) {
			while (right < s.length && p(s[right])) right++;
		}

		return [left, right, s.substring(left, right)];
	}

	function emitLeft(pos: number, acc: string[]): string[] {
		if (s[pos + 1] != '.') return acc;
		let [l, , e] = emitAt(pos, Direction.Left);
		if (e == "") return acc;
		acc.unshift(e);
		if (l <= 1 || s[l - 1] != '.') {
			return acc;
		}
		return emitLeft(l - 2, acc);
	}

	function emitRight(pos: number, acc: string[]): string[] {
		if (s[pos - 1] != '.') return acc;
		let [, r, e] = emitAt(pos, Direction.Right);
		if (e == "") return acc;
		acc.push(e);
		if (r >= s.length - 1 || s[r] != '.') {
			return acc;
		}
		return emitRight(r + 1, acc);
	}

	function isQualifier(s: string) {
		return s.length > 0 && isLarge(s[0]);
	}

	function findParenthesis(pos: number): [number, number] {
		let left = pos;
		while (left > 0 && s[left - 1] != '(') left--;
		let right = pos + 1;
		while (right < s.length && s[right] != ')') right++;
		return [left, right];
	}
	let [lP, rP] = findParenthesis(pos0);
	let s0 = s, pos0_ = pos0;
	s = s.slice(lP, rP);
	pos0 -= lP;
	console.log("%d => %d; %s => %s", pos0_, pos0, s0, s);
	console.log("identifier: %s", insertAndShift(s, "\u0333", pos0));

	// if the cursor is `.` which separates qualified names with an identifier/operator
	if (s[pos0] == "." && pos0 >= 1 && pos0 <= s.length - 1) {
		let isQualifierOnLeft = isIdentChar(s[pos0 - 1]) && isQualifier(emitAt(pos0 - 1, Direction.Left)[2]);
		if (isQualifierOnLeft && (isIdentChar(s[pos0 + 1]) || isSymbol(s[pos0 + 1]))) {
			console.log("cursor is at a qualifier separator");
			pos0--;
		}
	}

	let [l, r, e] = emitAt(pos0, Direction.Bidirectional);
	if (e == "") return ["", [["e == ''", `${l}`, `${r}`]]];

	if (e[0] == '.') {
		if (isQualifier(emitAt(l - 1, Direction.Left)[2])) {
			console.log("e[0] is not a part of the identifier, but rather a qualifier separator", e);
			e = e.substring(1);
			l++;
		}
	}

	let lA = emitLeft(l - 2, []);
	let rA = isQualifier(e) ? emitRight(r + 1, []) : [];

	if (lA.length == 0 && rA.length == 0) {
		return [e, [["lA.length == 0 && rA.length == 0"]]];
	}
	let a = [...lA, e, ...rA];
	let a1 = a.slice(a.findIndex(p => isLarge(p[0])));
	return [a1.join("."), [lA, [e], rA, a, a1]];
}

class XDefinitionProvider implements vscode.DefinitionProvider {
	async provideDefinition(
		document: vscode.TextDocument,
		position: vscode.Position,
		token: vscode.CancellationToken
	): Promise<vscode.Definition> {
		// let range = document.getWordRangeAtPosition(position);
		// let word = document.getText(range);
		let [word,] = identifier(document.lineAt(position.line).text, position.character);
		if (word == "") {
			[word,] = identifier(document.lineAt(position.line).text, position.character - 1);
		}
		if (word == "") {
			return Promise.resolve([]);
		}
		
		console.log("identifier: %s", word);
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

export async function activate(context: vscode.ExtensionContext) {
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
			let body = { dir: workspacePath };
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

	let workspaceFolders = vscode.workspace.workspaceFolders;
	if (workspaceFolders) {
		let workspacePath = workspaceFolders[0].uri.fsPath;
		let symlink = path.join(workspacePath, "./.repos");
		await createSymlink(serverRepos, symlink);
	}

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
