#!/usr/bin/env node

import { spawn } from 'child_process';

// Test the MCP server manually
function testMCPServer() {
  const server = spawn('node', ['dist/cli-index.js']);
  
  // Test read_sexp tool
  const readRequest = {
    jsonrpc: '2.0',
    id: 1,
    method: 'tools/call',
    params: {
      name: 'read_sexp',
      arguments: {
        filePath: '/tmp/test-new-sexp.rkt'
      }
    }
  };

  let output = '';
  
  server.stdout.on('data', (data) => {
    output += data.toString();
    console.log('Server output:', data.toString());
  });

  server.stderr.on('data', (data) => {
    console.error('Server error:', data.toString());
  });

  server.on('close', (code) => {
    console.log(`Server exited with code ${code}`);
  });

  // Send the request
  server.stdin.write(JSON.stringify(readRequest) + '\n');
  
  // Close after a short delay
  setTimeout(() => {
    server.kill();
  }, 2000);
}

testMCPServer();