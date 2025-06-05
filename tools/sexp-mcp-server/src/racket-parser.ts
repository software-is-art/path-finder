/**
 * Interface to the Racket parser for S-expression parsing
 */

import { spawn } from 'child_process';
import { promises as fs } from 'fs';
import path from 'path';
import { SExpression, SExpressionTree, ParseResult } from './types.js';

export class RacketParser {
  private racketScriptPath: string;

  constructor(projectRoot: string) {
    this.racketScriptPath = path.join(projectRoot, 'tools', 'sexp-mcp-server', 'racket', 'parse-sexp.rkt');
  }

  /**
   * Parse a Racket file into an S-expression tree
   */
  async parseFile(filePath: string): Promise<ParseResult> {
    try {
      // Check if file exists
      await fs.access(filePath);
      
      // Use Racket to parse the file
      const result = await this.runRacketParser(filePath);
      
      if (result.success && result.output) {
        const tree = this.convertRacketOutputToTree(result.output, filePath);
        return {
          success: true,
          tree
        };
      } else {
        return {
          success: false,
          error: result.error || 'Unknown parsing error'
        };
      }
    } catch (error) {
      return {
        success: false,
        error: `Failed to parse file: ${error instanceof Error ? error.message : String(error)}`
      };
    }
  }

  /**
   * Parse a string containing S-expressions
   */
  async parseString(content: string): Promise<ParseResult> {
    try {
      // Create temporary file
      const tempFile = path.join('/tmp', `sexp-${Date.now()}.rkt`);
      await fs.writeFile(tempFile, content);
      
      try {
        const result = await this.parseFile(tempFile);
        await fs.unlink(tempFile); // Clean up
        return result;
      } catch (error) {
        await fs.unlink(tempFile); // Clean up on error too
        throw error;
      }
    } catch (error) {
      return {
        success: false,
        error: `Failed to parse string: ${error instanceof Error ? error.message : String(error)}`
      };
    }
  }

  /**
   * Run the Racket parser script
   */
  private async runRacketParser(filePath: string): Promise<{ success: boolean; output?: any; error?: string }> {
    return new Promise((resolve) => {
      const racket = spawn('racket', [this.racketScriptPath, filePath]);
      
      let stdout = '';
      let stderr = '';
      
      racket.stdout.on('data', (data) => {
        stdout += data.toString();
      });
      
      racket.stderr.on('data', (data) => {
        stderr += data.toString();
      });
      
      racket.on('close', (code) => {
        if (code === 0) {
          try {
            const output = JSON.parse(stdout);
            resolve({ success: true, output });
          } catch (error) {
            resolve({ 
              success: false, 
              error: `Failed to parse Racket output: ${error instanceof Error ? error.message : String(error)}` 
            });
          }
        } else {
          resolve({ 
            success: false, 
            error: stderr || `Racket parser exited with code ${code}` 
          });
        }
      });
      
      racket.on('error', (error) => {
        resolve({ 
          success: false, 
          error: `Failed to run Racket parser: ${error.message}` 
        });
      });
    });
  }

  /**
   * Convert Racket parser output to our S-expression tree format
   */
  private convertRacketOutputToTree(racketOutput: any, filePath?: string): SExpressionTree {
    const convertExpression = (expr: any): SExpression => {
      if (typeof expr === 'string' || typeof expr === 'number' || typeof expr === 'boolean') {
        return {
          type: 'atom',
          value: expr
        };
      } else if (Array.isArray(expr)) {
        return {
          type: 'list',
          children: expr.map(convertExpression)
        };
      } else if (expr && typeof expr === 'object') {
        // Handle more complex Racket structures
        if (expr.type === 'symbol') {
          return {
            type: 'atom',
            value: expr.value
          };
        } else if (expr.type === 'list') {
          return {
            type: 'list',
            children: expr.elements ? expr.elements.map(convertExpression) : []
          };
        }
      }
      
      // Fallback for unknown structures
      return {
        type: 'atom',
        value: String(expr)
      };
    };

    return {
      root: convertExpression(racketOutput),
      filePath,
      source: 'racket-parser'
    };
  }
}