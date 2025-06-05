#!/usr/bin/env node

/**
 * S-Expression MCP Server - CLI Wrapper Version
 * Provides tools for structural S-expression editing using Racket CLI
 */

import { Server } from '@modelcontextprotocol/sdk/server/index.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';
import {
  CallToolRequestSchema,
  ListToolsRequestSchema,
  Tool,
} from '@modelcontextprotocol/sdk/types.js';

import { RacketCliWrapper } from './racket-cli-wrapper.js';

class SExpMcpServer {
  private server: Server;
  private racketCli: RacketCliWrapper;

  constructor() {
    this.racketCli = new RacketCliWrapper();
    
    this.server = new Server(
      {
        name: 'sexp-mcp-server',
        version: '2.0.0',
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
            name: 'format_sexp',
            description: 'Format an S-expression file with proper indentation',
            inputSchema: {
              type: 'object',
              properties: {
                filePath: {
                  type: 'string',
                  description: 'Path to the Racket/Lisp file to format',
                },
              },
              required: ['filePath'],
            },
          },
          {
            name: 'modify_sexp',
            description: 'Modify an S-expression at a specific path in the file',
            inputSchema: {
              type: 'object',
              properties: {
                filePath: {
                  type: 'string',
                  description: 'Path to the Racket/Lisp file to modify',
                },
                path: {
                  type: 'array',
                  items: { type: 'number' },
                  description: 'Path to the expression to modify (array of indices)',
                },
                newExpr: {
                  type: 'string',
                  description: 'The new S-expression as a string (e.g., "(+ 1 2)")',
                },
              },
              required: ['filePath', 'path', 'newExpr'],
            },
          },
          {
            name: 'wrap_sexp',
            description: 'Wrap an S-expression at a specific path with a new symbol',
            inputSchema: {
              type: 'object',
              properties: {
                filePath: {
                  type: 'string',
                  description: 'Path to the Racket/Lisp file to modify',
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
              required: ['filePath', 'path', 'wrapperSymbol'],
            },
          },
          {
            name: 'validate_sexp',
            description: 'Validate that a file contains valid S-expressions',
            inputSchema: {
              type: 'object',
              properties: {
                filePath: {
                  type: 'string',
                  description: 'Path to the Racket/Lisp file to validate',
                },
              },
              required: ['filePath'],
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
            return await this.handleReadSexp(args as { filePath: string });
          
          case 'format_sexp':
            return await this.handleFormatSexp(args as { filePath: string });
          
          case 'modify_sexp':
            return await this.handleModifySexp(args as { filePath: string; path: number[]; newExpr: string });
          
          case 'wrap_sexp':
            return await this.handleWrapSexp(args as { filePath: string; path: number[]; wrapperSymbol: string });
          
          case 'validate_sexp':
            return await this.handleValidateSexp(args as { filePath: string });
          
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

  private async handleReadSexp(args: { filePath: string }) {
    const result = await this.racketCli.readSexp(args.filePath);
    
    return {
      content: [
        {
          type: 'text',
          text: JSON.stringify({
            success: result.success,
            filePath: args.filePath,
            sexp: result.result || null,
            error: result.error || null,
          }, null, 2),
        },
      ],
    };
  }

  private async handleFormatSexp(args: { filePath: string }) {
    const result = await this.racketCli.formatSexp(args.filePath);
    
    return {
      content: [
        {
          type: 'text',
          text: JSON.stringify({
            success: result.success,
            filePath: args.filePath,
            formatted: result.result || null,
            error: result.error || null,
          }, null, 2),
        },
      ],
    };
  }

  private async handleModifySexp(args: { filePath: string; path: number[]; newExpr: string }) {
    const result = await this.racketCli.modifySexp(args.filePath, args.path, args.newExpr);
    
    return {
      content: [
        {
          type: 'text',
          text: JSON.stringify({
            success: result.success,
            filePath: args.filePath,
            modified: result.result || null,
            error: result.error || null,
          }, null, 2),
        },
      ],
    };
  }

  private async handleWrapSexp(args: { filePath: string; path: number[]; wrapperSymbol: string }) {
    const result = await this.racketCli.wrapSexp(args.filePath, args.path, args.wrapperSymbol);
    
    return {
      content: [
        {
          type: 'text',
          text: JSON.stringify({
            success: result.success,
            filePath: args.filePath,
            wrapped: result.result || null,
            error: result.error || null,
          }, null, 2),
        },
      ],
    };
  }

  private async handleValidateSexp(args: { filePath: string }) {
    const isValid = await this.racketCli.validateFile(args.filePath);
    
    return {
      content: [
        {
          type: 'text',
          text: JSON.stringify({
            success: true,
            filePath: args.filePath,
            valid: isValid,
          }, null, 2),
        },
      ],
    };
  }

  async run() {
    const transport = new StdioServerTransport();
    await this.server.connect(transport);
    console.error('S-Expression MCP Server (CLI version) running on stdio');
  }
}

// Start the server
if (import.meta.url === `file://${process.argv[1]}`) {
  const server = new SExpMcpServer();
  server.run().catch((error) => {
    console.error('Failed to start server:', error);
    process.exit(1);
  });
}

export { SExpMcpServer };