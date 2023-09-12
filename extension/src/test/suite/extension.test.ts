import * as assert from 'assert';

// You can import and use all API from the 'vscode' module
// as well as import your extension to test it
import * as vscode from 'vscode';
import * as ext from '../../extension';
import * as util from 'util';

suite('Extension Test Suite', () => {
	vscode.window.showInformationMessage('Start all tests.');

	suite('identifier', () => {
		// (.=), .= 			  => .=
		// (Prelude..), Prelude.. => Prelude..
		// Prelude.flip             => Prelude.flip
		// Data.List.map 		  	  => Data.List.map
		// Data.List 				  => Data.List
		// map 					  => map
		// Data 					  => Data
		// a.b 					  => b (if the cursor is on b)
		// (a).(b) 				  => b (if the cursor is on b)
		// A.a'b+c 					  => A.a'b (if the cursor is on A.a'b)

		// pos: -1 => any pos should produce expected output
		let cases = [
			{ input: "(.=)", pos: 1, output: ".=" },
			{ input: ".=", pos: -1, output: ".=" },
			{ input: "(Prelude..)", pos: 1, output: "Prelude.." },
			{ input: "Prelude..", pos: -1, output: "Prelude.." },
			{ input: "Prelude.flip", pos: -1, output: "Prelude.flip" },
			{ input: "Data.List.map", pos: -1, output: "Data.List.map" },
			{ input: "Data.List", pos: -1, output: "Data.List" },
			{ input: "map", pos: -1, output: "map" },
			{ input: "Data", pos: -1, output: "Data" },
			{ input: "a.b", pos: 0, output: "a" },
			{ input: "a.b", pos: 1, output: "." }, // ?
			{ input: "a.b", pos: 2, output: "b" },
			{ input: "(a).(b)", pos: 1, output: "a" },
			{ input: "(a).(b)", pos: 2, output: "a" }, // ?
			{ input: "(a).(b)", pos: 4, output: "b" }, // ?
			{ input: "(a).(b)", pos: 5, output: "b" },

			{ input: "A.a'b+c", pos: 0, output: "A.a'b" },
			{ input: "A.a'b+c", pos: 1, output: "A.a'b" },
			{ input: "A.a'b+c", pos: 2, output: "A.a'b" },
			{ input: "A.a'b+c", pos: 3, output: "A.a'b" },
			{ input: "A.a'b+c", pos: 4, output: "A.a'b" },

			{ input: "a Prelude.. b", pos: 0, output: "a" },
			{ input: "a Prelude.. b", pos: 2, output: "Prelude.." },
			{ input: "a Prelude.. b", pos: 8, output: "Prelude.." },
			{ input: "a Prelude.. b", pos: 9, output: "Prelude.." },
			{ input: "a Prelude.. b", pos: 10, output: "Prelude.." },
			{ input: "a Prelude.. b", pos: 12, output: "b" },

			{ input: "a M... b", pos: 0, output: "a" },
			{ input: "a M... b", pos: 2, output: "M..." },
			{ input: "a M... b", pos: 3, output: "M..." },
			{ input: "a M... b", pos: 4, output: "M..." },
			{ input: "a M... b", pos: 5, output: "M..." },
			{ input: "a M... b", pos: 7, output: "b" },
			{ input: "a M... b", pos: 1, output: "" },
			{ input: "a M... b", pos: 6, output: "" },

			{ input: "a..b", pos: 0, output: "a" },
			{ input: "a..b", pos: 1, output: ".." },
			{ input: "a..b", pos: 2, output: ".." },
			{ input: "a..b", pos: 3, output: "b" },

			{ input: "Right c", pos: 0, output: "Right" },
			{ input: "Right c", pos: 3, output: "Right" },
			{ input: "Right c", pos: 6, output: "c" },
			{ input: "     Right c", pos: 9, output: "Right" },
			{ input: "    Right c -> return c", pos: 8, output: "Right" },

			{ input: "import Control.Exception.Safe (tryAny)", pos: 28, output: "Control.Exception.Safe" },
		];

		cases.forEach((c) => {
			let t = (i: number) => {
				let tag = `identifier(${ext.insertAndShift(c.input, '\u0333', i + 1)}, ${i}) => ${c.output}`;
				test(tag, () => {
					let [result, di] = ext.identifier(c.input, i);
					let diS = "[" + di.map((d) => "[" + d.join(",") + "]").join(",") + "]";
					console.log(util.format("%s => `%s`, di=%s", tag, result, diS));
					assert.strictEqual(result, c.output);
				});
			};

			if (c.pos == -1) {
				for (let i = 0; i < c.input.length; i++) {
					t(i);
				}
				return;
			}

			t(c.pos);
		});
	});
});
