# TheyChat

TheyChat is a fault-tolerant chat system built with Erlang. It uses a client/server architecture and a supervisor/worker design pattern to ensure reliability and fault-tolerance.

Repository: https://github.com/LuisSerapicos/SISMD_P2_1230196_ERLANG_CHAT

## Features

- Clients can join/leave servers and send/receive messages.
- The server node is where clients can chat by sharing messages.
- The router is responsible for monitoring and solving server failures, and to save the state of the server, so when a new server instance is created by the router, it can recover the state of the failed server.

## Error Handling

Error handling is possible when:
- Router fails
- Server fails
- Client fails

When a server fails, the router will notice it and will assign a new server to the client.

When a client fails, the server notices it and updates the client list, so it will stop sending messages to that client.

## Modules

- `client.erl`: Handles sending and receiving messages, and joining and leaving servers.
- `server.erl`: Handles chat messages and maintains a list of connected clients.
- `router.erl`: Monitors multiple server processes.

## Getting Started

### Prerequisites

- Erlang/OTP 27

### Testing the Application

1. Clone the repository
2. Run the Erlang shell in the project directory
3. Compile the modules with `c(Module).` for each module
4. Start the router with `{ok, RouterPid} = router:start().`
5. Start a server with `{ok, ServerPid} = server:start(serverName).`
6. Begin monitoring servers with `router:monitor_servers([{serverName, 'serverNode@machineName'}, {serverName2, 'serverNode2@machineName'}]).`
7. Start a client `client:start(clientName).`
8. Connect the client to a server `client:connect(serverName, 'serverNode@machineName', clientName).`
9. Send a message with `client:send_msg(clientName, serverName, 'serverNode@machineName', "message").`
10. Leave the server with `client:leave(serverName, 'serverNode@machineName', clientName).`
11. Repeat steps 6-9 to add more clients to the server
12. To test the fault-tolerance, kill the server process with `exit(whereis(serverName), kill).` and see the router create a new server process.
13. Check router logs to see the server recovery process and use `observer:start().` in the server node to check the server process.

## Built With

* [Erlang](https://www.erlang.org/) - The programming language used.
