import http from 'http';
import { createTextEvent, createDoneEvent } from '@copilot-extensions/preview-sdk';

const server = http.createServer(async (req, res) => {
  if (req.method === 'GET') {
    res.writeHead(200, { 'Content-Type': 'text/plain' });
    res.end('Hello from Copilot extension server!\n');
  } else {
    res.write(createTextEvent('Hello world from Amdocs!\n'));
    res.write(createTextEvent('This is my first copilot extension'));
    res.end(createDoneEvent());
  }
});

const PORT = 3000;
server.listen(PORT, () => {
  console.log(`Copilot extension server running on port ${PORT}`);
});