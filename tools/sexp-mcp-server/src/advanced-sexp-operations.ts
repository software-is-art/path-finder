/**
 * Advanced S-expression operations inspired by paredit and other Lisp editors
 */

import { SExpression, SExpressionTree, SPath, ModifyResult } from './types.js';
import { SExpOperations } from './sexp-operations.js';

export interface NavigationResult {
  success: boolean;
  path?: SPath;
  error?: string;
}

export interface SelectionResult {
  success: boolean;
  startPath?: SPath;
  endPath?: SPath;
  expressions?: SExpression[];
  error?: string;
}

export class AdvancedSExpOperations extends SExpOperations {

  // ============================================================================
  // PAREDIT-STYLE STRUCTURAL EDITING
  // ============================================================================

  /**
   * Slurp: Move the next expression into this list
   * (foo bar) baz -> (foo bar baz)
   */
  static slurpForward(tree: SExpressionTree, path: SPath): ModifyResult {
    try {
      const newTree = this.cloneTree(tree);
      const target = this.navigateToPath(newTree, path);
      
      if (!target || target.type !== 'list') {
        return { success: false, error: 'Target must be a list for slurping' };
      }

      // Find the next sibling of this list
      if (path.length === 0) {
        return { success: false, error: 'Cannot slurp at root level' };
      }

      const parentInfo = this.getParent(newTree, path);
      if (!parentInfo) {
        return { success: false, error: 'Cannot find parent for slurping' };
      }

      const { parent, index } = parentInfo;
      if (!parent.children || index >= parent.children.length - 1) {
        return { success: false, error: 'No next sibling to slurp' };
      }

      // Move the next sibling into this list
      const nextSibling = parent.children[index + 1];
      target.children = target.children || [];
      target.children.push(this.cloneExpression(nextSibling));
      parent.children.splice(index + 1, 1);

      return { success: true, tree: newTree };

    } catch (error) {
      return { 
        success: false, 
        error: `Slurp forward failed: ${error instanceof Error ? error.message : String(error)}` 
      };
    }
  }

  /**
   * Slurp backward: Move the previous expression into this list
   * foo (bar baz) -> (foo bar baz)
   */
  static slurpBackward(tree: SExpressionTree, path: SPath): ModifyResult {
    try {
      const newTree = this.cloneTree(tree);
      const target = this.navigateToPath(newTree, path);
      
      if (!target || target.type !== 'list') {
        return { success: false, error: 'Target must be a list for slurping' };
      }

      if (path.length === 0) {
        return { success: false, error: 'Cannot slurp at root level' };
      }

      const parentInfo = this.getParent(newTree, path);
      if (!parentInfo) {
        return { success: false, error: 'Cannot find parent for slurping' };
      }

      const { parent, index } = parentInfo;
      if (!parent.children || index <= 0) {
        return { success: false, error: 'No previous sibling to slurp' };
      }

      // Move the previous sibling into this list at the beginning
      const prevSibling = parent.children[index - 1];
      target.children = target.children || [];
      target.children.unshift(this.cloneExpression(prevSibling));
      parent.children.splice(index - 1, 1);

      return { success: true, tree: newTree };

    } catch (error) {
      return { 
        success: false, 
        error: `Slurp backward failed: ${error instanceof Error ? error.message : String(error)}` 
      };
    }
  }

  /**
   * Barf: Move the last expression out of this list
   * (foo bar baz) -> (foo bar) baz
   */
  static barfForward(tree: SExpressionTree, path: SPath): ModifyResult {
    try {
      const newTree = this.cloneTree(tree);
      const target = this.navigateToPath(newTree, path);
      
      if (!target || target.type !== 'list' || !target.children || target.children.length === 0) {
        return { success: false, error: 'Target must be a non-empty list for barfing' };
      }

      if (path.length === 0) {
        return { success: false, error: 'Cannot barf at root level' };
      }

      const parentInfo = this.getParent(newTree, path);
      if (!parentInfo) {
        return { success: false, error: 'Cannot find parent for barfing' };
      }

      const { parent, index } = parentInfo;
      if (!parent.children) {
        return { success: false, error: 'Parent has no children' };
      }

      // Move the last child out of this list
      const lastChild = target.children.pop()!;
      parent.children.splice(index + 1, 0, lastChild);

      return { success: true, tree: newTree };

    } catch (error) {
      return { 
        success: false, 
        error: `Barf forward failed: ${error instanceof Error ? error.message : String(error)}` 
      };
    }
  }

  /**
   * Barf backward: Move the first expression out of this list
   * (foo bar baz) -> foo (bar baz)
   */
  static barfBackward(tree: SExpressionTree, path: SPath): ModifyResult {
    try {
      const newTree = this.cloneTree(tree);
      const target = this.navigateToPath(newTree, path);
      
      if (!target || target.type !== 'list' || !target.children || target.children.length === 0) {
        return { success: false, error: 'Target must be a non-empty list for barfing' };
      }

      if (path.length === 0) {
        return { success: false, error: 'Cannot barf at root level' };
      }

      const parentInfo = this.getParent(newTree, path);
      if (!parentInfo) {
        return { success: false, error: 'Cannot find parent for barfing' };
      }

      const { parent, index } = parentInfo;
      if (!parent.children) {
        return { success: false, error: 'Parent has no children' };
      }

      // Move the first child out of this list
      const firstChild = target.children.shift()!;
      parent.children.splice(index, 0, firstChild);

      return { success: true, tree: newTree };

    } catch (error) {
      return { 
        success: false, 
        error: `Barf backward failed: ${error instanceof Error ? error.message : String(error)}` 
      };
    }
  }

  /**
   * Splice: Remove the wrapping parentheses, moving children up one level
   * (outer (inner foo bar) baz) -> (outer foo bar baz)
   */
  static splice(tree: SExpressionTree, path: SPath): ModifyResult {
    try {
      const newTree = this.cloneTree(tree);
      const target = this.navigateToPath(newTree, path);
      
      if (!target || target.type !== 'list') {
        return { success: false, error: 'Target must be a list for splicing' };
      }

      if (path.length === 0) {
        return { success: false, error: 'Cannot splice root expression' };
      }

      const parentInfo = this.getParent(newTree, path);
      if (!parentInfo) {
        return { success: false, error: 'Cannot find parent for splicing' };
      }

      const { parent, index } = parentInfo;
      if (!parent.children) {
        return { success: false, error: 'Parent has no children' };
      }

      // Replace this list with its children
      const children = target.children || [];
      parent.children.splice(index, 1, ...children);

      return { success: true, tree: newTree };

    } catch (error) {
      return { 
        success: false, 
        error: `Splice failed: ${error instanceof Error ? error.message : String(error)}` 
      };
    }
  }

  /**
   * Raise: Replace the parent with this expression
   * (outer (inner target other) baz) -> (outer target baz)
   */
  static raise(tree: SExpressionTree, path: SPath): ModifyResult {
    try {
      const newTree = this.cloneTree(tree);
      const target = this.navigateToPath(newTree, path);
      
      if (!target) {
        return { success: false, error: 'Target not found' };
      }

      if (path.length === 0) {
        return { success: false, error: 'Cannot raise root expression' };
      }

      if (path.length === 1) {
        // Raising to root level
        newTree.root = this.cloneExpression(target);
        return { success: true, tree: newTree };
      }

      const grandparentPath = path.slice(0, -2);
      const parentIndex = path[path.length - 2];
      const grandparent = this.navigateToPath(newTree, grandparentPath);
      
      if (!grandparent || grandparent.type !== 'list' || !grandparent.children) {
        return { success: false, error: 'Cannot find grandparent for raising' };
      }

      // Replace the parent with this expression
      grandparent.children[parentIndex] = this.cloneExpression(target);

      return { success: true, tree: newTree };

    } catch (error) {
      return { 
        success: false, 
        error: `Raise failed: ${error instanceof Error ? error.message : String(error)}` 
      };
    }
  }

  // ============================================================================
  // SMART NAVIGATION
  // ============================================================================

  /**
   * Find the path to the next S-expression at the same level
   */
  static navigateForward(tree: SExpressionTree, path: SPath): NavigationResult {
    try {
      if (path.length === 0) {
        return { success: false, error: 'Cannot navigate forward from root' };
      }

      const parentPath = path.slice(0, -1);
      const index = path[path.length - 1];
      const parent = this.navigateToPath(tree, parentPath);
      
      if (!parent || parent.type !== 'list' || !parent.children) {
        return { success: false, error: 'Parent is not a list' };
      }

      if (index >= parent.children.length - 1) {
        return { success: false, error: 'No next sibling' };
      }

      return { 
        success: true, 
        path: [...parentPath, index + 1] 
      };

    } catch (error) {
      return { 
        success: false, 
        error: `Navigate forward failed: ${error instanceof Error ? error.message : String(error)}` 
      };
    }
  }

  /**
   * Find the path to the previous S-expression at the same level
   */
  static navigateBackward(tree: SExpressionTree, path: SPath): NavigationResult {
    try {
      if (path.length === 0) {
        return { success: false, error: 'Cannot navigate backward from root' };
      }

      const parentPath = path.slice(0, -1);
      const index = path[path.length - 1];
      
      if (index <= 0) {
        return { success: false, error: 'No previous sibling' };
      }

      return { 
        success: true, 
        path: [...parentPath, index - 1] 
      };

    } catch (error) {
      return { 
        success: false, 
        error: `Navigate backward failed: ${error instanceof Error ? error.message : String(error)}` 
      };
    }
  }

  /**
   * Navigate up to the parent expression
   */
  static navigateUp(tree: SExpressionTree, path: SPath): NavigationResult {
    if (path.length === 0) {
      return { success: false, error: 'Already at root' };
    }

    return { 
      success: true, 
      path: path.slice(0, -1) 
    };
  }

  /**
   * Navigate down to the first child expression
   */
  static navigateDown(tree: SExpressionTree, path: SPath): NavigationResult {
    try {
      const target = this.navigateToPath(tree, path);
      
      if (!target) {
        return { success: false, error: 'Target not found' };
      }

      if (target.type !== 'list' || !target.children || target.children.length === 0) {
        return { success: false, error: 'Target has no children' };
      }

      return { 
        success: true, 
        path: [...path, 0] 
      };

    } catch (error) {
      return { 
        success: false, 
        error: `Navigate down failed: ${error instanceof Error ? error.message : String(error)}` 
      };
    }
  }

  // ============================================================================
  // SELECTION AND EXPANSION
  // ============================================================================

  /**
   * Expand selection to encompass the parent expression
   */
  static expandSelection(tree: SExpressionTree, currentPath: SPath): NavigationResult {
    if (currentPath.length === 0) {
      return { success: false, error: 'Selection already at maximum (root)' };
    }

    return { 
      success: true, 
      path: currentPath.slice(0, -1) 
    };
  }

  /**
   * Contract selection to the first child
   */
  static contractSelection(tree: SExpressionTree, currentPath: SPath): NavigationResult {
    return this.navigateDown(tree, currentPath);
  }

  /**
   * Select all expressions at the current level
   */
  static selectSiblings(tree: SExpressionTree, path: SPath): SelectionResult {
    try {
      if (path.length === 0) {
        return { 
          success: true, 
          startPath: [], 
          endPath: [], 
          expressions: [tree.root] 
        };
      }

      const parentPath = path.slice(0, -1);
      const parent = this.navigateToPath(tree, parentPath);
      
      if (!parent || parent.type !== 'list' || !parent.children) {
        return { success: false, error: 'Parent is not a list' };
      }

      return {
        success: true,
        startPath: [...parentPath, 0],
        endPath: [...parentPath, parent.children.length - 1],
        expressions: parent.children
      };

    } catch (error) {
      return { 
        success: false, 
        error: `Select siblings failed: ${error instanceof Error ? error.message : String(error)}` 
      };
    }
  }
}