import axios from 'axios';
import * as vscode from 'vscode';
import path = require('path/posix');
import fs = require('node:fs');
import { ChildProcess, exec, spawn } from 'child_process';
import { homedir } from 'os'
import * as util from 'util';
import { FileHandle } from 'node:fs/promises';

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

///

function resolvePath(p: string): string {
	if (p.startsWith('~')) {
		return path.normalize(path.join(userHomeDir, p.slice(1)));
	}
	return p;
}

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
		outputChannel.appendLine(util.format('repos %s -> %s failed: %s', src, dst, error));
	}
}

const haskellExtensionID = "haskell.haskell";

function isMainHaskellExtensionActive(): boolean {
	const extension = vscode.extensions.getExtension(haskellExtensionID);
	return extension?.isActive ?? false;
}

///

let port = 0;
let stdout: FileHandle | null = null, stderr: FileHandle | null = null;
let server: ChildProcess | null = null;

async function sendHeartbeat() {
	let data = await fs.promises.readFile(serverPortF, "utf8");
	port = parseInt(data, 10);
	if (port <= 0) return false;

	let res = await axios.
		post(`http://localhost:${port}/ping`, null).
		catch(function (error) {
			outputChannel.appendLine(util.format(error));
			return null;
		});
	outputChannel.appendLine(util.format("heartbeat: %s", res?.status));
	return res != null;
}

async function startServerIfRequired() {
	try { if (await sendHeartbeat()) return; }
	catch (error) { outputChannel.appendLine(util.format(error)); }

	outputChannel.appendLine(util.format("starting server"));

	if (stdout != null) { await stdout.close(); stdout = null; }
	if (stderr != null) { await stderr.close(); stderr = null; }
	if (server != null) { server.kill(); server = null; stopServer(); }

	let conf = vscode.workspace.getConfiguration('hs-gtd');
	let serverArgs = conf.get<string[]>("server.args") ?? [];
	let packageArgs = conf.get<string[]>("package.args") ?? [];
	let args = serverArgs.concat(packageArgs.length > 0 ? ["--package", ...packageArgs] : []);

	const ac = new AbortController();
	const { signal } = ac;
	setTimeout(() => ac.abort(), 5000);

	const watcher = fs.promises.watch(serverPortF, { signal });

	stdout = await fs.promises.open(path.join(serverRoot, 'stdout.log'), 'a');
	stderr = await fs.promises.open(path.join(serverRoot, 'stderr.log'), 'a');
	server = spawn(
		serverExe,
		args,
		{
			detached: true,
			stdio: ['ignore', stdout.fd, stderr.fd],
		}
	);
	server.unref();

	try { for await (const { } of watcher) break; }
	catch (err) { outputChannel.appendLine(util.format("serverPortF watcher: %s", err)); }

	await sendHeartbeat();
}

async function stopServer() {
	let pid: number = -1;
	let data = await fs.promises.readFile(serverPidF, "utf8");
	pid = parseInt(data, 10);
	if (pid <= 0) return;
	process.kill(pid);
}

///

// `identifier` returns the longest identifier that contains the given position.
// Implemented here instead of server because of server-client communication model. Has to be moved to the server eventually (LSP).
//
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
// More examples can be found in tests.
/* This 	Lexes as this
https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-140002
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
		return isASCII(c) ? c == '_' || c >= 'a' && c <= 'z' : upLower.test(c);
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

		// figure out 'working directory' in multi-root workspace
		let doc = vscode.window.activeTextEditor?.document;
		if (!doc) return Promise.resolve([]);
		let docF = vscode.workspace.getWorkspaceFolder(doc.uri);
		if (!docF) return Promise.resolve([]);
		let wd = docF.uri.fsPath;
		let repos = path.join(wd, "./.repos");
		let file = document.uri.fsPath;

		// figure out the word under the cursor
		let [word,] = identifier(document.lineAt(position.line).text, position.character);
		if (word == "") {
			[word,] = identifier(document.lineAt(position.line).text, position.character - 1);
		}
		if (word == "") {
			return Promise.resolve([]);
		}
		outputChannel.appendLine(util.format("getting a definition for %s...", word));

		// send a request to the server
		await startServerIfRequired();
		let body = {
			workDir: wd,
			file: file,
			word: word
		};
		let res = await axios.
			post(`http://localhost:${port}/definition`, body).
			catch(function (error) {
				outputChannel.appendLine(util.format("%s -> %s", body, error));
				return { "data": {} };
			});
		let data = res.data;
		if (data.err != undefined && data.err != "") {
			outputChannel.appendLine(util.format("%s -> err=%s (data=%s)", word, data.err, JSON.stringify(data)));
			return Promise.resolve([]);
		}
		if (data.srcSpan == undefined) {
			outputChannel.appendLine(util.format("%s -> no srcSpan (data=%s)", word, JSON.stringify(data)));
			return Promise.resolve([]);
		}
		outputChannel.appendLine(util.format("response = %s", data));

		// HLS BUG #1: in case definition is located at `repos` directory, rewrite the path to local symlink to `repos` (named `./{wd}/.repos`) to prevent Haskell VS Code extension from spawning a new instance of the HLS
		// HLS interoperability: if HLS is provided via `haskell.haskell` extension, then this extension should not provide local resolutions until they become the same as the ones provided by `haskell.haskell` extension
		let wordSourcePathO = data.srcSpan.sourceSpanFileName;
		let wordSourcePath;
		if (isParentOf(wd, wordSourcePathO)) {
			wordSourcePath = wordSourcePathO;
			if (isMainHaskellExtensionActive()) {
				let conf = vscode.workspace.getConfiguration('hs-gtd');
				let disableLocDefs = conf.get<boolean>("extension.disable-local-definitions-when-hls-is-active") ?? true;
				if (disableLocDefs) {
					outputChannel.appendLine(util.format("HLS is active, disabling local definitions; got %s", wordSourcePathO));
					return Promise.resolve([]);
				}
			}
		} else if (wordSourcePathO == file) {
			wordSourcePath = wordSourcePathO;
		} else if (isParentOf(serverRepos, wordSourcePathO)) {
			wordSourcePath = path.resolve(repos, path.relative(serverRepos, wordSourcePathO))
		} else {
			outputChannel.appendLine(util.format("BUG: wordSourcePathO (%s) is neither original file (%s) nor is present in neither serverRepos (%s) nor current work directory (%s)", wordSourcePathO, file, serverRepos, wd));
			return Promise.resolve([]);
		}
		wordSourcePath = path.normalize(wordSourcePath);
		outputChannel.appendLine(util.format("path rewritten: %s -> %s", wordSourcePathO, wordSourcePath));
		let wordSourceURI = vscode.Uri.file(path.normalize(wordSourcePath));

		// return the actual definition location
		let line = data.srcSpan.sourceSpanStartLine - 1; // 0-based line number
		let character = data.srcSpan.sourceSpanStartColumn - 1; // 0-based character position
		let definitionPosition = new vscode.Position(line, character);
		let definitionLocation = new vscode.Location(wordSourceURI, definitionPosition);
		return Promise.resolve(definitionLocation);
	}
}

export async function activate(context: vscode.ExtensionContext) {
	outputChannel = vscode.window.createOutputChannel("haskell-gtd");

	outputChannel.appendLine(userHomeDir);

	// reset current working directory cache whenever a Cabal or Haskell file is saved
	context.subscriptions.push(
		vscode.workspace.onDidSaveTextDocument(async (document: vscode.TextDocument) => {
			let doc = vscode.window.activeTextEditor?.document;
			if (!doc) return Promise.resolve([]);
			let docF = vscode.workspace.getWorkspaceFolder(doc.uri);
			if (!docF) return Promise.resolve([]);
			let wd = docF.uri.fsPath;
			let repos = path.join(wd, "./.repos");

			outputChannel.appendLine(util.format("saved languageId=%s: %s", document.languageId, document.uri.fsPath));
			if (isParentOf(repos, document.uri.fsPath) ||
				!(document.languageId == "haskell" || document.languageId == "cabal") ||
				!isParentOf(wd, document.uri.fsPath)) return;
			outputChannel.appendLine(util.format("resetting workspace extension cache"));

			await startServerIfRequired();
			let body = { dir: wd };
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

	// see HLS BUG #1
	let workspaceFolders = vscode.workspace.workspaceFolders;
	for (let wd in workspaceFolders) {
		let repos = path.join(wd, "./.repos");
		await createSymlink(serverRepos, repos);

		let files = vscode.workspace.getConfiguration('files');
		let watcherExclude: any = files.get('watcherExclude');
		watcherExclude['**/path/to/exclude'] = true;
		files.update('watcherExclude', watcherExclude, vscode.ConfigurationTarget.WorkspaceFolder);
		let exclude: any = files.get('exclude');
		exclude['**/path/to/exclude'] = true;
		files.update('exclude', exclude, vscode.ConfigurationTarget.WorkspaceFolder);

		let search = vscode.workspace.getConfiguration('search');
		let excludeS: any = search.get('exclude');
		excludeS['**/path/to/exclude'] = true;
		search.update('exclude', excludeS, vscode.ConfigurationTarget.WorkspaceFolder);
	}

	context.subscriptions.push(vscode.commands.registerCommand('hs-gtd.server.restart', stopServer));

	vscode.workspace.onDidChangeConfiguration((e) => {
		if (!e.affectsConfiguration('hs-gtd')) {
			return;
		}
		let conf = vscode.workspace.getConfiguration('hs-gtd', vscode.window.activeTextEditor?.document?.uri);
		serverRoot = conf.get<string>('server.root') ?? path.join(userHomeDir, "/.local/share/haskell-gtd-extension-server-root");
		serverRoot = resolvePath(serverRoot);
		serverExe = conf.get<string>('server.path') ?? "haskell-gtd-server";
		serverExe = conf.get<string>('package.path') ?? "haskell-gtd-server";
		if (!path.isAbsolute(serverExe)) {
			serverExe = path.normalize(path.join(serverRoot, serverExe));
		}
		refreshSubPaths();

		if (e.affectsConfiguration("hs-gtd.package.args") || e.affectsConfiguration("hs-gtd.server.args")) {
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
