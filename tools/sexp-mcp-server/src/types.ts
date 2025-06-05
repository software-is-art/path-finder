/**
 * S-Expression types for structural editing
 */

export type SPath = number[];

export interface SExpression {
  type: 'atom' | 'list';
  value?: string | number | boolean;
  children?: SExpression[];
  metadata?: {
    line?: number;
    column?: number;
    source?: string;
  };
}

export interface SExpressionTree {
  root: SExpression;
  source?: string;
  filePath?: string;
}

export interface ParseResult {
  success: boolean;
  tree?: SExpressionTree;
  error?: string;
  line?: number;
  column?: number;
}

export interface ModifyResult {
  success: boolean;
  tree?: SExpressionTree;
  error?: string;
}

export interface ValidationResult {
  valid: boolean;
  errors?: string[];
}

// MCP Tool request/response types
export interface ReadSexpRequest {
  filePath: string;
}

export interface ReadSexpResponse {
  success: boolean;
  tree?: SExpressionTree;
  error?: string;
}

export interface ModifySexpRequest {
  tree: SExpressionTree;
  path: SPath;
  newSubtree: SExpression;
}

export interface ModifySexpResponse {
  success: boolean;
  tree?: SExpressionTree;
  error?: string;
}

export interface InsertSexpRequest {
  tree: SExpressionTree;
  path: SPath;
  index: number;
  newSubtree: SExpression;
}

export interface InsertSexpResponse {
  success: boolean;
  tree?: SExpressionTree;
  error?: string;
}

export interface DeleteSexpRequest {
  tree: SExpressionTree;
  path: SPath;
}

export interface DeleteSexpResponse {
  success: boolean;
  tree?: SExpressionTree;
  error?: string;
}

export interface WrapSexpRequest {
  tree: SExpressionTree;
  path: SPath;
  wrapperSymbol: string;
}

export interface WrapSexpResponse {
  success: boolean;
  tree?: SExpressionTree;
  error?: string;
}

export interface FormatSexpRequest {
  tree: SExpressionTree;
  indentSize?: number;
}

export interface FormatSexpResponse {
  success: boolean;
  formatted?: string;
  error?: string;
}

export interface ValidateSexpRequest {
  tree: SExpressionTree;
}

export interface ValidateSexpResponse {
  valid: boolean;
  errors?: string[];
}