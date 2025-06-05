#!/usr/bin/env node

/**
 * S-Expression MCP Server
 * Provides tools for structural S-expression editing
 */

import { Server } from '@modelcontextprotocol/sdk/server/index.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';
import {
  CallToolRequestSchema,
  ListToolsRequestSchema,
  Tool,
} from '@modelcontextprotocol/sdk/types.js';
import path from 'path';
import { fileURLToPath } from 'url';
import { dirname } from 'path';

import { RacketParser } from './racket-parser.js';
import { SExpOperations } from './sexp-operations.js';
import { AdvancedSExpOperations } from './advanced-sexp-operations.js';
import { SExpFormatter } from './formatter.js';
import {
  ReadSexpRequest,
  ModifySexpRequest,
  InsertSexpRequest,
  DeleteSexpRequest,
  WrapSexpRequest,
  FormatSexpRequest,
  ValidateSexpRequest,
} from './types.js';

class SExpMcpServer {
  private server: Server;
  private parser: RacketParser;
  private projectRoot: string;

  constructor() {
    this.projectRoot = process.env.PROJECT_ROOT || path.resolve(__dirname, '../../..');
    this.parser = new RacketParser(this.projectRoot);
    
    this.server = new Server(
      {
        name: 'sexp-mcp-server',
        version: '1.0.0',
      },
      {
        capabilities: {
          tools: {},
        },
      }
    );

    this.setupHandlers();
  }

  private setupHandlers() {
    // List available tools
    this.server.setRequestHandler(ListToolsRequestSchema, async () => {
      return {
        tools: [
          {
            name: 'read_sexp',
            description: 'Parse a Racket/Lisp file into an S-expression tree',
            inputSchema: {
              type: 'object',
              properties: {
                filePath: {
                  type: 'string',
                  description: 'Path to the Racket/Lisp file to parse',
                },
              },
              required: ['filePath'],
            },
          },
          {
            name: 'modify_sexp',
            description: 'Modify an S-expression at a specific path in the tree',
            inputSchema: {
              type: 'object',
              properties: {
                tree: {
                  type: 'object',
                  description: 'The S-expression tree to modify',
                },
                path: {
                  type: 'array',
                  items: { type: 'number' },
                  description: 'Path to the expression to modify (array of indices)',
                },
                newSubtree: {
                  type: 'object',
                  description: 'The new S-expression to replace the existing one',
                },
              },
              required: ['tree', 'path', 'newSubtree'],
            },
          },
          {
            name: 'insert_sexp',
            description: 'Insert a new S-expression at a specific path and index',
            inputSchema: {
              type: 'object',
              properties: {
                tree: {
                  type: 'object',
                  description: 'The S-expression tree to modify',
                },
                path: {
                  type: 'array',
                  items: { type: 'number' },
                  description: 'Path to the list where to insert (array of indices)',
                },
                index: {
                  type: 'number',
                  description: 'Index within the list where to insert',
                },
                newSubtree: {
                  type: 'object',
                  description: 'The new S-expression to insert',
                },
              },
              required: ['tree', 'path', 'index', 'newSubtree'],
            },
          },
          {
            name: 'delete_sexp',
            description: 'Delete an S-expression at a specific path',
            inputSchema: {
              type: 'object',
              properties: {
                tree: {
                  type: 'object',
                  description: 'The S-expression tree to modify',
                },
                path: {
                  type: 'array',
                  items: { type: 'number' },
                  description: 'Path to the expression to delete (array of indices)',
                },
              },
              required: ['tree', 'path'],
            },
          },
          {
            name: 'wrap_sexp',
            description: 'Wrap an S-expression at a specific path with a new list',
            inputSchema: {
              type: 'object',
              properties: {
                tree: {
                  type: 'object',
                  description: 'The S-expression tree to modify',
                },
                path: {
                  type: 'array',
                  items: { type: 'number' },
                  description: 'Path to the expression to wrap (array of indices)',
                },
                wrapperSymbol: {
                  type: 'string',
                  description: 'Symbol to use as the wrapper (e.g., "if", "let")',
                },
              },
              required: ['tree', 'path', 'wrapperSymbol'],
            },
          },
          {
            name: 'format_sexp',
            description: 'Format an S-expression tree into a pretty-printed string',
            inputSchema: {
              type: 'object',
              properties: {
                tree: {
                  type: 'object',
                  description: 'The S-expression tree to format',
                },
                indentSize: {
                  type: 'number',
                  description: 'Number of spaces for indentation (default: 2)',
                },
              },
              required: ['tree'],
            },
          },
          {
            name: 'validate_sexp',
            description: 'Validate the structural integrity of an S-expression tree',
            inputSchema: {
              type: 'object',
              properties: {
                tree: {
                  type: 'object',
                  description: 'The S-expression tree to validate',
                },
              },
              required: ['tree'],
            },
          },
          {
            name: 'slurp_forward',
            description: 'Slurp: Move the next expression into this list (foo bar) baz -> (foo bar baz)',
            inputSchema: {
              type: 'object',
              properties: {
                tree: {
                  type: 'object',
                  description: 'The S-expression tree to modify',
                },
                path: {
                  type: 'array',
                  items: { type: 'number' },
                  description: 'Path to the list expression (array of indices)',
                },
              },
              required: ['tree', 'path'],
            },
          },
          {
            name: 'slurp_backward',
            description: 'Slurp backward: Move the previous expression into this list foo (bar baz) -> (foo bar baz)',
            inputSchema: {
              type: 'object',
              properties: {
                tree: {
                  type: 'object',
                  description: 'The S-expression tree to modify',
                },
                path: {
                  type: 'array',
                  items: { type: 'number' },
                  description: 'Path to the list expression (array of indices)',
                },
              },
              required: ['tree', 'path'],
            },
          },
          {
            name: 'barf_forward',
            description: 'Barf: Move the last expression out of this list (foo bar baz) -> (foo bar) baz',
            inputSchema: {
              type: 'object',
              properties: {
                tree: {
                  type: 'object',
                  description: 'The S-expression tree to modify',
                },
                path: {
                  type: 'array',
                  items: { type: 'number' },
                  description: 'Path to the list expression (array of indices)',
                },
              },
              required: ['tree', 'path'],
            },
          },
          {
            name: 'barf_backward',
            description: 'Barf backward: Move the first expression out of this list (foo bar baz) -> foo (bar baz)',
            inputSchema: {
              type: 'object',
              properties: {
                tree: {
                  type: 'object',
                  description: 'The S-expression tree to modify',
                },
                path: {
                  type: 'array',
                  items: { type: 'number' },
                  description: 'Path to the list expression (array of indices)',
                },
              },
              required: ['tree', 'path'],
            },
          },
          {
            name: 'splice',
            description: 'Splice: Remove wrapping parentheses, moving children up one level (outer (inner foo bar) baz) -> (outer foo bar baz)',
            inputSchema: {
              type: 'object',
              properties: {
                tree: {
                  type: 'object',
                  description: 'The S-expression tree to modify',
                },
                path: {
                  type: 'array',
                  items: { type: 'number' },
                  description: 'Path to the list expression to splice (array of indices)',
                },
              },
              required: ['tree', 'path'],
            },
          },
          {
            name: 'raise',
            description: 'Raise: Replace the parent with this expression (outer (inner target other) baz) -> (outer target baz)',
            inputSchema: {
              type: 'object',
              properties: {
                tree: {
                  type: 'object',
                  description: 'The S-expression tree to modify',
                },
                path: {
                  type: 'array',
                  items: { type: 'number' },
                  description: 'Path to the expression to raise (array of indices)',
                },
              },
              required: ['tree', 'path'],
            },
          },
          {
            name: 'navigate_forward',
            description: 'Navigate to the next S-expression at the same level',
            inputSchema: {
              type: 'object',
              properties: {
                tree: {
                  type: 'object',
                  description: 'The S-expression tree',
                },
                path: {
                  type: 'array',
                  items: { type: 'number' },
                  description: 'Current path (array of indices)',
                },
              },
              required: ['tree', 'path'],
            },
          },
          {
            name: 'navigate_backward',
            description: 'Navigate to the previous S-expression at the same level',
            inputSchema: {
              type: 'object',
              properties: {
                tree: {
                  type: 'object',
                  description: 'The S-expression tree',
                },
                path: {
                  type: 'array',
                  items: { type: 'number' },
                  description: 'Current path (array of indices)',
                },
              },
              required: ['tree', 'path'],
            },
          },
          {
            name: 'navigate_up',
            description: 'Navigate up to the parent expression',
            inputSchema: {
              type: 'object',
              properties: {
                tree: {
                  type: 'object',
                  description: 'The S-expression tree',
                },
                path: {
                  type: 'array',
                  items: { type: 'number' },
                  description: 'Current path (array of indices)',
                },
              },
              required: ['tree', 'path'],
            },
          },
          {
            name: 'navigate_down',
            description: 'Navigate down to the first child expression',
            inputSchema: {
              type: 'object',
              properties: {
                tree: {
                  type: 'object',
                  description: 'The S-expression tree',
                },
                path: {
                  type: 'array',
                  items: { type: 'number' },
                  description: 'Current path (array of indices)',
                },
              },
              required: ['tree', 'path'],
            },
          },
          {
            name: 'expand_selection',
            description: 'Expand selection to encompass the parent expression',
            inputSchema: {
              type: 'object',
              properties: {
                tree: {
                  type: 'object',
                  description: 'The S-expression tree',
                },
                path: {
                  type: 'array',
                  items: { type: 'number' },
                  description: 'Current path (array of indices)',
                },
              },
              required: ['tree', 'path'],
            },
          },
          {
            name: 'contract_selection',
            description: 'Contract selection to the first child',
            inputSchema: {
              type: 'object',
              properties: {
                tree: {
                  type: 'object',
                  description: 'The S-expression tree',
                },
                path: {
                  type: 'array',
                  items: { type: 'number' },
                  description: 'Current path (array of indices)',
                },
              },
              required: ['tree', 'path'],
            },
          },
          {
            name: 'select_siblings',
            description: 'Select all expressions at the current level',
            inputSchema: {
              type: 'object',
              properties: {
                tree: {
                  type: 'object',
                  description: 'The S-expression tree',
                },
                path: {
                  type: 'array',
                  items: { type: 'number' },
                  description: 'Current path (array of indices)',
                },
              },
              required: ['tree', 'path'],
            },
          },
        ] as Tool[],
      };
    });

    // Handle tool calls
    this.server.setRequestHandler(CallToolRequestSchema, async (request) => {
      const { name, arguments: args } = request.params;

      try {
        switch (name) {
          case 'read_sexp':
            return await this.handleReadSexp(args as unknown as ReadSexpRequest);
          
          case 'modify_sexp':
            return await this.handleModifySexp(args as unknown as ModifySexpRequest);
          
          case 'insert_sexp':
            return await this.handleInsertSexp(args as unknown as InsertSexpRequest);
          
          case 'delete_sexp':
            return await this.handleDeleteSexp(args as unknown as DeleteSexpRequest);
          
          case 'wrap_sexp':
            return await this.handleWrapSexp(args as unknown as WrapSexpRequest);
          
          case 'format_sexp':
            return await this.handleFormatSexp(args as unknown as FormatSexpRequest);
          
          case 'validate_sexp':
            return await this.handleValidateSexp(args as unknown as ValidateSexpRequest);
          
          // Advanced S-expression operations
          case 'slurp_forward':
            return await this.handleSlurpForward(args);
          
          case 'slurp_backward':
            return await this.handleSlurpBackward(args);
          
          case 'barf_forward':
            return await this.handleBarfForward(args);
          
          case 'barf_backward':
            return await this.handleBarfBackward(args);
          
          case 'splice':
            return await this.handleSplice(args);
          
          case 'raise':
            return await this.handleRaise(args);
          
          case 'navigate_forward':
            return await this.handleNavigateForward(args);
          
          case 'navigate_backward':
            return await this.handleNavigateBackward(args);
          
          case 'navigate_up':
            return await this.handleNavigateUp(args);
          
          case 'navigate_down':
            return await this.handleNavigateDown(args);
          
          case 'expand_selection':
            return await this.handleExpandSelection(args);
          
          case 'contract_selection':
            return await this.handleContractSelection(args);
          
          case 'select_siblings':
            return await this.handleSelectSiblings(args);
          
          default:
            throw new Error(`Unknown tool: ${name}`);
        }
      } catch (error) {
        return {
          content: [
            {
              type: 'text',
              text: `Error: ${error instanceof Error ? error.message : String(error)}`,
            },
          ],
        };
      }
    });
  }

  private async handleReadSexp(args: ReadSexpRequest) {
    const result = await this.parser.parseFile(args.filePath);
    
    return {
      content: [
        {
          type: 'text',
          text: JSON.stringify(result, null, 2),
        },
      ],
    };
  }

  private async handleModifySexp(args: ModifySexpRequest) {
    const result = SExpOperations.modifyAtPath(args.tree, args.path, args.newSubtree);
    
    return {
      content: [
        {
          type: 'text',
          text: JSON.stringify(result, null, 2),
        },
      ],
    };
  }

  private async handleInsertSexp(args: InsertSexpRequest) {
    const result = SExpOperations.insertAtPath(args.tree, args.path, args.index, args.newSubtree);
    
    return {
      content: [
        {
          type: 'text',
          text: JSON.stringify(result, null, 2),
        },
      ],
    };
  }

  private async handleDeleteSexp(args: DeleteSexpRequest) {
    const result = SExpOperations.deleteAtPath(args.tree, args.path);
    
    return {
      content: [
        {
          type: 'text',
          text: JSON.stringify(result, null, 2),
        },
      ],
    };
  }

  private async handleWrapSexp(args: WrapSexpRequest) {
    const result = SExpOperations.wrapAtPath(args.tree, args.path, args.wrapperSymbol);
    
    return {
      content: [
        {
          type: 'text',
          text: JSON.stringify(result, null, 2),
        },
      ],
    };
  }

  private async handleFormatSexp(args: FormatSexpRequest) {
    const formatter = new SExpFormatter({
      indentSize: args.indentSize || 2,
    });
    
    const formatted = formatter.format(args.tree);
    
    return {
      content: [
        {
          type: 'text',
          text: JSON.stringify({
            success: true,
            formatted,
          }, null, 2),
        },
      ],
    };
  }

  private async handleValidateSexp(args: ValidateSexpRequest) {
    const result = SExpOperations.validate(args.tree);
    
    return {
      content: [
        {
          type: 'text',
          text: JSON.stringify(result, null, 2),
        },
      ],
    };
  }

  // Advanced S-expression operation handlers
  private async handleSlurpForward(args: any) {
    const result = AdvancedSExpOperations.slurpForward(args.tree, args.path);
    
    return {
      content: [
        {
          type: 'text',
          text: JSON.stringify(result, null, 2),
        },
      ],
    };
  }

  private async handleSlurpBackward(args: any) {
    const result = AdvancedSExpOperations.slurpBackward(args.tree, args.path);
    
    return {
      content: [
        {
          type: 'text',
          text: JSON.stringify(result, null, 2),
        },
      ],
    };
  }

  private async handleBarfForward(args: any) {
    const result = AdvancedSExpOperations.barfForward(args.tree, args.path);
    
    return {
      content: [
        {
          type: 'text',
          text: JSON.stringify(result, null, 2),
        },
      ],
    };
  }

  private async handleBarfBackward(args: any) {
    const result = AdvancedSExpOperations.barfBackward(args.tree, args.path);
    
    return {
      content: [
        {
          type: 'text',
          text: JSON.stringify(result, null, 2),
        },
      ],
    };
  }

  private async handleSplice(args: any) {
    const result = AdvancedSExpOperations.splice(args.tree, args.path);
    
    return {
      content: [
        {
          type: 'text',
          text: JSON.stringify(result, null, 2),
        },
      ],
    };
  }

  private async handleRaise(args: any) {
    const result = AdvancedSExpOperations.raise(args.tree, args.path);
    
    return {
      content: [
        {
          type: 'text',
          text: JSON.stringify(result, null, 2),
        },
      ],
    };
  }

  private async handleNavigateForward(args: any) {
    const result = AdvancedSExpOperations.navigateForward(args.tree, args.path);
    
    return {
      content: [
        {
          type: 'text',
          text: JSON.stringify(result, null, 2),
        },
      ],
    };
  }

  private async handleNavigateBackward(args: any) {
    const result = AdvancedSExpOperations.navigateBackward(args.tree, args.path);
    
    return {
      content: [
        {
          type: 'text',
          text: JSON.stringify(result, null, 2),
        },
      ],
    };
  }

  private async handleNavigateUp(args: any) {
    const result = AdvancedSExpOperations.navigateUp(args.tree, args.path);
    
    return {
      content: [
        {
          type: 'text',
          text: JSON.stringify(result, null, 2),
        },
      ],
    };
  }

  private async handleNavigateDown(args: any) {
    const result = AdvancedSExpOperations.navigateDown(args.tree, args.path);
    
    return {
      content: [
        {
          type: 'text',
          text: JSON.stringify(result, null, 2),
        },
      ],
    };
  }

  private async handleExpandSelection(args: any) {
    const result = AdvancedSExpOperations.expandSelection(args.tree, args.path);
    
    return {
      content: [
        {
          type: 'text',
          text: JSON.stringify(result, null, 2),
        },
      ],
    };
  }

  private async handleContractSelection(args: any) {
    const result = AdvancedSExpOperations.contractSelection(args.tree, args.path);
    
    return {
      content: [
        {
          type: 'text',
          text: JSON.stringify(result, null, 2),
        },
      ],
    };
  }

  private async handleSelectSiblings(args: any) {
    const result = AdvancedSExpOperations.selectSiblings(args.tree, args.path);
    
    return {
      content: [
        {
          type: 'text',
          text: JSON.stringify(result, null, 2),
        },
      ],
    };
  }

  async run() {
    const transport = new StdioServerTransport();
    await this.server.connect(transport);
    console.error('S-Expression MCP Server running on stdio');
  }
}

// Start the server
const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

if (import.meta.url === `file://${process.argv[1]}`) {
  const server = new SExpMcpServer();
  server.run().catch((error) => {
    console.error('Failed to start server:', error);
    process.exit(1);
  });
}

export { SExpMcpServer };