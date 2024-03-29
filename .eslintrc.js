/* eslint-disable array-bracket-newline, array-element-newline, quote-props */
module.exports = {
	"env": {
		"browser": true,
		"node": true
	},
	"extends": "eslint:recommended",
	"rules": {
		"accessor-pairs": "error",
		"array-bracket-newline": "error",
		"array-bracket-spacing": "error",
		"array-callback-return": "error",
		"array-element-newline": "error",
		"arrow-body-style": "error",
		"arrow-parens": "error",
		"arrow-spacing": "error",
		"block-scoped-var": "error",
		"block-spacing": [
			"error",
			"always"
		],
		"brace-style": [
			"error",
			"1tbs",
			{
				"allowSingleLine": true
			}
		],
		"callback-return": "error",
		"camelcase": "error",
		"capitalized-comments": "off",
		"class-methods-use-this": "error",
		"comma-dangle": "error",
		"comma-spacing": [
			"error",
			{
				"after": true,
				"before": false
			}
		],
		"comma-style": [
			"error",
			"last"
		],
		"complexity": "error",
		"computed-property-spacing": [
			"error",
			"never"
		],
		"consistent-return": "error",
		"consistent-this": "error",
		"curly": "error",
		"default-case": "warn",
		"dot-location": "error",
		"dot-notation": "error",
		"eol-last": "error",
		"eqeqeq": "error",
		"for-direction": "error",
		"func-call-spacing": "error",
		"func-name-matching": "error",
		"func-names": [
			"warn",
			"never"
		],
		"func-style": "off",
		"generator-star-spacing": "error",
		"getter-return": "error",
		"global-require": "error",
		"guard-for-in": "error",
		"handle-callback-err": "error",
		"id-blacklist": "error",
		"id-length": "off",
		"id-match": "error",
		"indent": ["warn", "tab"],
		"indent-legacy": "off",
		"init-declarations": "off",
		"jsx-quotes": "error",
		"key-spacing": "error",
		"keyword-spacing": [
			"error",
			{
				"after": true,
				"before": true
			}
		],
		"line-comment-position": "off",
		"linebreak-style": [
			"error"
		],
		"lines-around-comment": "off",
		"lines-around-directive": "error",
		"max-depth": "warn",
		"max-len": "off",
		"max-lines": "off",
		"max-nested-callbacks": "error",
		"max-params": "off",
		"max-statements": "off",
		"max-statements-per-line": "warn",
		"multiline-ternary": ["warn", "never"],
		"new-cap": "error",
		"new-parens": "error",
		"newline-after-var": 1,
		"newline-before-return": "off",
		"newline-per-chained-call": "off",
		"no-alert": "error",
		"no-array-constructor": "error",
		"no-await-in-loop": "error",
		"no-bitwise": "off", // modif
		"no-buffer-constructor": "error",
		"no-caller": "error",
		"no-catch-shadow": "error",
		"no-compare-neg-zero": "error",
		"no-cond-assign": "error",
		"no-confusing-arrow": "error",
		"no-constant-condition": "warn",
		"no-continue": "warn",
		"no-control-regex": "warn",
		"no-div-regex": "error",
		"no-dupe-args": "warn",
		"no-dupe-keys": "warn",
		"no-duplicate-case": "warn",
		"no-duplicate-imports": "error",
		"no-else-return": "error",
		"no-empty-function": "warn",
		"no-eq-null": "error",
		"no-eval": "error",
		"no-ex-assign": "warn",
		"no-extend-native": "warn",
		"no-extra-bind": "error",
		"no-extra-boolean-cast": "warn",
		"no-extra-label": "error",
		"no-extra-parens": "off",
		"no-extra-semi": "warn",
		"no-floating-decimal": "error",
		"no-func-assign": "error",
		"no-implicit-coercion": "error",
		"no-implicit-globals": "off",
		"no-implied-eval": "error",
		"no-inline-comments": "off",
		"no-inner-declarations": [
			"error",
			"functions"
		],
		"no-invalid-this": "error",
		"no-invalid-regexp": "error",
		"no-irregular-whitespace": "warn",
		"no-iterator": "error",
		"no-label-var": "error",
		"no-labels": "error",
		"no-lone-blocks": "error",
		"no-lonely-if": "error",
		"no-loop-func": "error",
		"no-magic-numbers": "off",
		"no-mixed-operators": "off",
		"no-mixed-requires": "error",
		"no-mixed-spaces-and-tabs": "warn",
		"no-multi-assign": "error",
		"no-multi-spaces": [
			"error",
			{
				"ignoreEOLComments": false
			}
		],
		"no-multi-str": "error",
		"no-multiple-empty-lines": "warn",
		"no-native-reassign": "error",
		"no-negated-condition": "off",
		"no-negated-in-lhs": "error",
		"no-nested-ternary": "error",
		"no-new": "error",
		"no-new-func": "error",
		"no-new-object": "error",
		"no-new-require": "error",
		"no-new-wrappers": "error",
		"no-octal-escape": "error",
		"no-param-reassign": "off",
		"no-path-concat": "error",
		"no-plusplus": "off", // modif
		"no-process-env": "error",
		"no-process-exit": "error",
		"no-proto": "error",
		"no-prototype-builtins": "off",
		"no-regex-spaces": "warn",
		"no-restricted-globals": "error",
		"no-restricted-imports": "error",
		"no-restricted-modules": "error",
		"no-restricted-properties": "error",
		"no-restricted-syntax": "error",
		"no-return-assign": "error",
		"no-return-await": "error",
		"no-script-url": "error",
		"no-self-assign": "warn",
		"no-self-compare": "error",
		"no-sequences": "error",
		"no-shadow": "error",
		"no-shadow-restricted-names": "error",
		"no-spaced-func": "error",
		"no-sync": "error",
		"no-tabs": "off",
		"no-ternary": "off",
		"no-this-before-super": "warn",
		"no-throw-literal": "warn",
		"no-trailing-spaces": "error",
		"no-undef-init": "error",
		"no-undefined": "off",
		"no-underscore-dangle": "error",
		"no-unmodified-loop-condition": "warn",
		"no-unneeded-ternary": "error",
		"no-unused-expressions": "error",
		"no-use-before-define": "error",
		"no-useless-call": "error",
		"no-useless-computed-key": "error",
		"no-useless-concat": "warn",
		"no-useless-constructor": "error",
		"no-useless-rename": "error",
		"no-useless-return": "error",
		"no-var": "off",
		"no-void": "error",
		"no-warning-comments": "warn",
		"no-whitespace-before-property": "error",
		"no-with": "error",
		"nonblock-statement-body-position": "error",
		"object-curly-newline": ["error", { "ObjectExpression": { "multiline": true, "minProperties": 1 } }],
		"object-curly-spacing": ["warn", "always", { "arraysInObjects": false, "objectsInObjects": false }],
		"object-property-newline": [
			"error",
			{
				"allowMultiplePropertiesPerLine": false
			}
		],
		"object-shorthand": "off",
		"one-var": "warn",
		"one-var-declaration-per-line": "warn",
		"operator-assignment": [
			"error",
			"always"
		],
		"operator-linebreak": [
			"error",
			"before"
		],
		"padded-blocks": ["warn", "never"],
		"padding-line-between-statements": "error",
		"prefer-arrow-callback": "off",
		"prefer-const": "error",
		"prefer-destructuring": [
			"error",
			{
				"array": false,
				"object": false
			}
		],
		"prefer-numeric-literals": "error",
		"prefer-promise-reject-errors": "error",
		"prefer-reflect": "off",
		"prefer-rest-params": "off",
		"prefer-spread": "off",
		"prefer-template": "off",
		"quote-props": [2, "as-needed", { "keywords": true, "unnecessary": true }],
		"quotes": ["warn", "double", { "avoidEscape": true }],
		"radix": "error",
		"require-await": "error",
		"require-jsdoc": "off",
		"rest-spread-spacing": "error",
		"semi": "error",
		"semi-spacing": "error",
		"semi-style": [
			"error",
			"last"
		],
		"sort-imports": "error",
		"sort-keys": "off",
		"sort-vars": "off",
		"space-before-blocks": "error",
		"space-before-function-paren": ["warn", { "anonymous": "always", "named": "never", "asyncArrow": "always" }],
		"space-in-parens": [
			"error",
			"never"
		],
		"space-infix-ops": "error",
		"space-unary-ops": ["error", { "words": true, "nonwords": false }],
		"spaced-comment": [
			"warn",
			"always"
		],
		"strict": [
			"error",
			"global"
		],
		"switch-colon-spacing": "error",
		"symbol-description": "error",
		"template-curly-spacing": "error",
		"template-tag-spacing": "error",
		"unicode-bom": [
			"error",
			"never"
		],
		"valid-jsdoc": "warn",
		"vars-on-top": "warn",
		"wrap-iife": "error",
		"wrap-regex": "error",
		"yield-star-spacing": "error",
		"yoda": [
			"error",
			"never"
		]
	}
};
/* eslint-enable array-bracket-newline, array-element-newline, quote-props */
