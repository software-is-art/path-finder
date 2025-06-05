/**
 * Racket CLI wrapper for S-expression operations
 */

import { spawn } from 'child_process';
import path from 'path';
import { fileURLToPath } from 'url';
import { dirname } from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

export interface SExpResult {
  success: boolean;
  result?: string;
  error?: string;
}

export class RacketCliWrapper {
  private racketScriptPath: string;

  constructor() {
    this.racketScriptPath = path.join(__dirname, '..', 'racket', 'simple-sexp-cli.rkt');
  }

  /**
   * Execute a Racket CLI command
   */
  private async executeCommand(args: string[]): Promise<SExpResult> {
    return new Promise((resolve) => {
      const racket = spawn('racket', [this.racketScriptPath, ...args]);
      
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
            const result = JSON.parse(stdout.trim()) as SExpResult;
            resolve(result);
          } catch (error) {
            resolve({
              success: false,
              error: `Failed to parse Racket output: ${error instanceof Error ? error.message : String(error)}`
            });
          }
        } else {
          resolve({
            success: false,
            error: stderr || `Racket CLI exited with code ${code}`
          });
        }
      });
      
      racket.on('error', (error) => {
        resolve({
          success: false,
          error: `Failed to run Racket CLI: ${error.message}`
        });
      });
    });
  }

  /**
   * Read and parse S-expression from file
   */
  async readSexp(filePath: string): Promise<SExpResult> {
    return this.executeCommand(['read', filePath]);
  }

  /**
   * Format S-expression file
   */
  async formatSexp(filePath: string): Promise<SExpResult> {
    return this.executeCommand(['format', filePath]);
  }

  /**
   * Modify S-expression at specific path
   */
  async modifySexp(filePath: string, path: number[], newExpr: string): Promise<SExpResult> {
    const pathJson = JSON.stringify(path);
    return this.executeCommand(['modify', filePath, pathJson, newExpr]);
  }

  /**
   * Wrap S-expression at path with new symbol
   */
  async wrapSexp(filePath: string, path: number[], wrapperSymbol: string): Promise<SExpResult> {
    const pathJson = JSON.stringify(path);
    return this.executeCommand(['wrap', filePath, pathJson, wrapperSymbol]);
  }

  /**
   * Validate that we can work with the file
   */
  async validateFile(filePath: string): Promise<boolean> {
    const result = await this.readSexp(filePath);
    return result.success;
  }
}