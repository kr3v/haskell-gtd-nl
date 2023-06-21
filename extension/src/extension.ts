import axios from 'axios';
import * as vscode from 'vscode';
import path = require('path/posix');

class XDefinitionProvider implements vscode.DefinitionProvider {
	async provideDefinition(
		document: vscode.TextDocument,
		position: vscode.Position,
		token: vscode.CancellationToken
	): Promise<vscode.Definition> {
		let range = document.getWordRangeAtPosition(position);
		let word = document.getText(range);

		console.log(word);

		if (vscode.workspace.workspaceFolders) {
			let workspaceFolder = vscode.workspace.workspaceFolders[0];
			let workspacePath = workspaceFolder.uri.fsPath;
			let docPath = document.uri.fsPath;
			let body = {
				workDir: workspacePath,
				file: docPath,
				word: word
			};
			let res = await axios.
				post('http://localhost:53465/definition', body).
				catch(function (error) {
					console.log("for body {body}");
					console.log(error);
					return { "data": {} };
				});
			let data = res.data;
			if (data.srcSpan === undefined) {
				return Promise.resolve([]);
			}

			let filePath = data.srcSpan.sourceSpanFileName;
			let fileUri = vscode.Uri.file(path.join(workspacePath, path.normalize(filePath)));

			let line = data.srcSpan.sourceSpanStartLine - 1; // 0-based line number
			let character = data.srcSpan.sourceSpanStartColumn - 1; // 0-based character position
			let definitionPosition = new vscode.Position(line, character);
			let definitionLocation = new vscode.Location(fileUri, definitionPosition);

			console.log(filePath);
			return Promise.resolve(definitionLocation);
		}
		return Promise.resolve([]);
	}
}

export function activate(context: vscode.ExtensionContext) {
	console.log('Congratulations, your extension "hs-gtd" is now active!');
	context.subscriptions.push(
		vscode.languages.registerDefinitionProvider(
			{ scheme: 'file', language: 'haskell' },
			new XDefinitionProvider()
		)
	);
}

// This method is called when your extension is deactivated
export function deactivate() { }
