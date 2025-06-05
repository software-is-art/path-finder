/**
 * Core S-expression tree operations
 */

import { SExpression, SExpressionTree, SPath, ModifyResult, ValidationResult } from './types.js';

export class SExpOperations {
  
  /**
   * Navigate to a specific path in the S-expression tree
   */
  static navigateToPath(tree: SExpressionTree, path: SPath): SExpression | null {
    let current = tree.root;
    
    for (const index of path) {
      if (current.type !== 'list' || !current.children) {
        return null; // Can't navigate further into non-list
      }
      
      if (index < 0 || index >= current.children.length) {
        return null; // Index out of bounds
      }
      
      current = current.children[index];
    }
    
    return current;
  }

  /**
   * Get the parent of an expression at a given path
   */
  static getParent(tree: SExpressionTree, path: SPath): { parent: SExpression; index: number } | null {
    if (path.length === 0) {
      return null; // Root has no parent
    }
    
    const parentPath = path.slice(0, -1);
    const index = path[path.length - 1];
    const parent = this.navigateToPath(tree, parentPath);
    
    if (!parent || parent.type !== 'list' || !parent.children) {
      return null;
    }
    
    return { parent, index };
  }

  /**
   * Deep clone an S-expression
   */
  static cloneExpression(expr: SExpression): SExpression {
    if (expr.type === 'atom') {
      return {
        type: 'atom',
        value: expr.value,
        metadata: expr.metadata ? { ...expr.metadata } : undefined
      };
    } else {
      return {
        type: 'list',
        children: expr.children ? expr.children.map(child => this.cloneExpression(child)) : [],
        metadata: expr.metadata ? { ...expr.metadata } : undefined
      };
    }
  }

  /**
   * Deep clone an entire tree
   */
  static cloneTree(tree: SExpressionTree): SExpressionTree {
    return {
      root: this.cloneExpression(tree.root),
      source: tree.source,
      filePath: tree.filePath
    };
  }

  /**
   * Modify an expression at a specific path
   */
  static modifyAtPath(tree: SExpressionTree, path: SPath, newExpression: SExpression): ModifyResult {
    try {
      const newTree = this.cloneTree(tree);
      
      if (path.length === 0) {
        // Replace root
        newTree.root = this.cloneExpression(newExpression);
        return { success: true, tree: newTree };
      }
      
      const parentInfo = this.getParent(newTree, path);
      if (!parentInfo) {
        return { success: false, error: 'Invalid path: parent not found' };
      }
      
      const { parent, index } = parentInfo;
      if (!parent.children || index < 0 || index >= parent.children.length) {
        return { success: false, error: 'Invalid path: index out of bounds' };
      }
      
      parent.children[index] = this.cloneExpression(newExpression);
      return { success: true, tree: newTree };
      
    } catch (error) {
      return { 
        success: false, 
        error: `Modification failed: ${error instanceof Error ? error.message : String(error)}` 
      };
    }
  }

  /**
   * Insert an expression at a specific path and index
   */
  static insertAtPath(tree: SExpressionTree, path: SPath, index: number, newExpression: SExpression): ModifyResult {
    try {
      const newTree = this.cloneTree(tree);
      const target = this.navigateToPath(newTree, path);
      
      if (!target) {
        return { success: false, error: 'Invalid path: target not found' };
      }
      
      if (target.type !== 'list') {
        return { success: false, error: 'Cannot insert into non-list expression' };
      }
      
      if (!target.children) {
        target.children = [];
      }
      
      if (index < 0 || index > target.children.length) {
        return { success: false, error: 'Invalid insertion index' };
      }
      
      target.children.splice(index, 0, this.cloneExpression(newExpression));
      return { success: true, tree: newTree };
      
    } catch (error) {
      return { 
        success: false, 
        error: `Insertion failed: ${error instanceof Error ? error.message : String(error)}` 
      };
    }
  }

  /**
   * Delete an expression at a specific path
   */
  static deleteAtPath(tree: SExpressionTree, path: SPath): ModifyResult {
    try {
      if (path.length === 0) {
        return { success: false, error: 'Cannot delete root expression' };
      }
      
      const newTree = this.cloneTree(tree);
      const parentInfo = this.getParent(newTree, path);
      
      if (!parentInfo) {
        return { success: false, error: 'Invalid path: parent not found' };
      }
      
      const { parent, index } = parentInfo;
      if (!parent.children || index < 0 || index >= parent.children.length) {
        return { success: false, error: 'Invalid path: index out of bounds' };
      }
      
      parent.children.splice(index, 1);
      return { success: true, tree: newTree };
      
    } catch (error) {
      return { 
        success: false, 
        error: `Deletion failed: ${error instanceof Error ? error.message : String(error)}` 
      };
    }
  }

  /**
   * Wrap an expression at a specific path with a new list
   */
  static wrapAtPath(tree: SExpressionTree, path: SPath, wrapperSymbol: string): ModifyResult {
    try {
      const newTree = this.cloneTree(tree);
      const target = this.navigateToPath(newTree, path);
      
      if (!target) {
        return { success: false, error: 'Invalid path: target not found' };
      }
      
      const originalExpression = this.cloneExpression(target);
      const wrapperExpression: SExpression = {
        type: 'list',
        children: [
          { type: 'atom', value: wrapperSymbol },
          originalExpression
        ]
      };
      
      if (path.length === 0) {
        // Wrapping root
        newTree.root = wrapperExpression;
        return { success: true, tree: newTree };
      }
      
      const parentInfo = this.getParent(newTree, path);
      if (!parentInfo) {
        return { success: false, error: 'Invalid path: parent not found' };
      }
      
      const { parent, index } = parentInfo;
      if (!parent.children || index < 0 || index >= parent.children.length) {
        return { success: false, error: 'Invalid path: index out of bounds' };
      }
      
      parent.children[index] = wrapperExpression;
      return { success: true, tree: newTree };
      
    } catch (error) {
      return { 
        success: false, 
        error: `Wrapping failed: ${error instanceof Error ? error.message : String(error)}` 
      };
    }
  }

  /**
   * Validate the structural integrity of an S-expression tree
   */
  static validate(tree: SExpressionTree): ValidationResult {
    const errors: string[] = [];
    
    const validateExpression = (expr: SExpression, path: string = 'root'): void => {
      if (!expr.type || (expr.type !== 'atom' && expr.type !== 'list')) {
        errors.push(`Invalid expression type at ${path}: ${expr.type}`);
        return;
      }
      
      if (expr.type === 'atom') {
        if (expr.children) {
          errors.push(`Atom at ${path} should not have children`);
        }
        if (expr.value === undefined || expr.value === null) {
          errors.push(`Atom at ${path} missing value`);
        }
      } else if (expr.type === 'list') {
        if (expr.value !== undefined) {
          errors.push(`List at ${path} should not have a value`);
        }
        if (expr.children) {
          expr.children.forEach((child, index) => {
            validateExpression(child, `${path}[${index}]`);
          });
        }
      }
    };
    
    validateExpression(tree.root);
    
    return {
      valid: errors.length === 0,
      errors: errors.length > 0 ? errors : undefined
    };
  }
}