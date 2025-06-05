/**
 * S-expression formatter for pretty-printing
 */

import { SExpression, SExpressionTree } from './types.js';

export interface FormatOptions {
  indentSize: number;
  maxLineLength: number;
  compactSimpleExpressions: boolean;
}

export class SExpFormatter {
  private options: FormatOptions;

  constructor(options: Partial<FormatOptions> = {}) {
    this.options = {
      indentSize: 2,
      maxLineLength: 80,
      compactSimpleExpressions: true,
      ...options
    };
  }

  /**
   * Format an S-expression tree into a string
   */
  format(tree: SExpressionTree): string {
    return this.formatExpression(tree.root, 0);
  }

  /**
   * Format a single expression
   */
  private formatExpression(expr: SExpression, depth: number): string {
    if (expr.type === 'atom') {
      return this.formatAtom(expr);
    } else {
      return this.formatList(expr, depth);
    }
  }

  /**
   * Format an atom expression
   */
  private formatAtom(expr: SExpression): string {
    const value = expr.value;
    
    if (typeof value === 'string') {
      // Check if it needs to be quoted
      if (this.needsQuoting(value)) {
        return `"${this.escapeString(value)}"`;
      } else {
        return value;
      }
    } else if (typeof value === 'number') {
      return String(value);
    } else if (typeof value === 'boolean') {
      return value ? '#t' : '#f';
    } else {
      return String(value);
    }
  }

  /**
   * Format a list expression
   */
  private formatList(expr: SExpression, depth: number): string {
    if (!expr.children || expr.children.length === 0) {
      return '()';
    }

    const children = expr.children;
    
    // Check if this should be formatted compactly
    if (this.shouldFormatCompactly(expr)) {
      const formatted = children.map(child => this.formatExpression(child, depth + 1)).join(' ');
      return `(${formatted})`;
    }

    // Multi-line formatting
    const indent = ' '.repeat(depth * this.options.indentSize);
    const childIndent = ' '.repeat((depth + 1) * this.options.indentSize);
    
    const formattedChildren = children.map(child => {
      const formatted = this.formatExpression(child, depth + 1);
      return `${childIndent}${formatted}`;
    });

    return `(\n${formattedChildren.join('\n')}\n${indent})`;
  }

  /**
   * Determine if a list should be formatted compactly on one line
   */
  private shouldFormatCompactly(expr: SExpression): boolean {
    if (!this.options.compactSimpleExpressions || !expr.children) {
      return false;
    }

    // Don't compact if there are too many children
    if (expr.children.length > 4) {
      return false;
    }

    // Don't compact if any child is a list
    if (expr.children.some(child => child.type === 'list')) {
      return false;
    }

    // Estimate the line length
    const estimatedLength = this.estimateLength(expr);
    return estimatedLength <= this.options.maxLineLength;
  }

  /**
   * Estimate the length of a formatted expression
   */
  private estimateLength(expr: SExpression): number {
    if (expr.type === 'atom') {
      return String(expr.value).length;
    } else if (expr.children) {
      const childrenLength = expr.children.reduce((sum, child) => sum + this.estimateLength(child), 0);
      const spacesLength = expr.children.length - 1; // spaces between children
      return 2 + childrenLength + spacesLength; // 2 for parentheses
    }
    return 2; // empty list
  }

  /**
   * Check if a string needs to be quoted
   */
  private needsQuoting(str: string): boolean {
    // Check for special characters, whitespace, or if it starts with a number
    return /[\s\(\)\[\]"';#]|^[0-9]/.test(str) || str === '';
  }

  /**
   * Escape special characters in a string
   */
  private escapeString(str: string): string {
    return str
      .replace(/\\/g, '\\\\')
      .replace(/"/g, '\\"')
      .replace(/\n/g, '\\n')
      .replace(/\t/g, '\\t')
      .replace(/\r/g, '\\r');
  }

  /**
   * Format with specific options for different contexts
   */
  static formatCompact(tree: SExpressionTree): string {
    const formatter = new SExpFormatter({
      indentSize: 2,
      maxLineLength: 120,
      compactSimpleExpressions: true
    });
    return formatter.format(tree);
  }

  static formatVerbose(tree: SExpressionTree): string {
    const formatter = new SExpFormatter({
      indentSize: 2,
      maxLineLength: 60,
      compactSimpleExpressions: false
    });
    return formatter.format(tree);
  }

  static formatMinimal(tree: SExpressionTree): string {
    const formatter = new SExpFormatter({
      indentSize: 1,
      maxLineLength: 200,
      compactSimpleExpressions: true
    });
    return formatter.format(tree);
  }
}