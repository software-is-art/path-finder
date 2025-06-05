#!/usr/bin/env node

/**
 * Test script for advanced S-expression operations
 */

import { AdvancedSExpOperations } from './dist/advanced-sexp-operations.js';

// Create a test S-expression tree: (outer (inner foo bar) baz qux)
const testTree = {
  root: {
    type: 'list',
    children: [
      { type: 'atom', value: 'outer' },
      {
        type: 'list',
        children: [
          { type: 'atom', value: 'inner' },
          { type: 'atom', value: 'foo' },
          { type: 'atom', value: 'bar' }
        ]
      },
      { type: 'atom', value: 'baz' },
      { type: 'atom', value: 'qux' }
    ]
  }
};

console.log('=== Advanced S-Expression Operations Test ===\n');

console.log('Original tree:');
console.log(JSON.stringify(testTree, null, 2));

// Test slurp forward on the inner list at path [1]
console.log('\n--- Test: Slurp Forward ---');
console.log('Operation: Move "baz" into the inner list');
console.log('Path: [1] (the inner list)');
const slurpResult = AdvancedSExpOperations.slurpForward(testTree, [1]);
console.log('Result:');
console.log(JSON.stringify(slurpResult, null, 2));

// Test navigation
console.log('\n--- Test: Navigate Forward ---');
console.log('Operation: Navigate from "foo" to "bar"');
console.log('Path: [1, 1] (foo) -> [1, 2] (bar)');
const navResult = AdvancedSExpOperations.navigateForward(testTree, [1, 1]);
console.log('Result:');
console.log(JSON.stringify(navResult, null, 2));

// Test selection expansion
console.log('\n--- Test: Expand Selection ---');
console.log('Operation: Expand selection from "foo" to encompass parent');
console.log('Path: [1, 1] (foo) -> [1] (inner list)');
const expandResult = AdvancedSExpOperations.expandSelection(testTree, [1, 1]);
console.log('Result:');
console.log(JSON.stringify(expandResult, null, 2));

// Test select siblings
console.log('\n--- Test: Select Siblings ---');
console.log('Operation: Select all children of inner list');
console.log('Path: [1, 1] (foo as reference point)');
const siblingsResult = AdvancedSExpOperations.selectSiblings(testTree, [1, 1]);
console.log('Result:');
console.log(JSON.stringify(siblingsResult, null, 2));

console.log('\n=== Test Complete ===');