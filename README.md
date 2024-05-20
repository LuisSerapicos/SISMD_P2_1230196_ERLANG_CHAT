# TheyChat

TheyChat is a fault-tolerant chat system built with Erlang. It uses a client/server architecture and a supervisor/worker design pattern to ensure reliability and fault-tolerance.

## Features

- Clients can join/leave servers and send/receive messages.
- The router is responsible for indicating to a client the PID of a server the client wishes to join and for monitoring and solving server failures.
- The server node is where clients can chat by sharing messages and also implements fail-safe features.

## Error Handling

Error handling is possible when:
- Router fails
- Server fails
- Client fails

When a client fails, the server must notice it and updates the client list so it will stop sending messages to that client.

## Modules

- `client.erl`: Handles sending and receiving messages, and joining and leaving servers.
- `server.erl`: Handles chat messages and maintains a list of connected clients.
- `router.erl`: Provides the PID of a server to a client and monitors the server processes.

## Getting Started

### Prerequisites

- Erlang/OTP 27

### Installing

1. Clone the repository
2. Run the Erlang shell in the project directory
3. Compile the modules with `c(Module).` for each module
4. Start the router with `router:start().`
5. Start a server with `server:start('serverName').`
6. Start a client with `client:start('clientName').`
7. Add the server to client with `client:add_remote('remoteMachineName').`
8. Send a message with `client:send_msg('clientName', 'serverName', 'remoteMachineName', 'message').`
9. Leave the server with `client:leave('clientName').`
10. Repeat steps 6-9 to add more clients to the server

## Built With

* [Erlang](https://www.erlang.org/) - The programming language used.
