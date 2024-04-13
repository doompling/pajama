#[repr(C)]
pub struct PjStr {
    buffer: *const i8,
    length: i64,
    max_length: i64,
}

#[repr(C)]
pub struct PjTcpServer {
    host: *mut PjStr,
    port: *mut PjStr,
    tcp_listener: *mut TcpListener,
    poll: *mut Poll,
    events: *mut Events,
    connections: *mut HashMap<Token, TcpStream>,
    conn_id: i64,
}

#[repr(C)]
pub struct PjTcpEvents {
    tcp_writable_fn: fn(&PjTcpConnection),
    tcp_data_received_fn: fn(&PjTcpConnection, &PjStr),
}

#[repr(C)]
pub struct PjTcpConnection {
    server: *mut PjTcpServer,
    tcp_stream: *mut TcpStream,
    event: *const Event,
}

#[no_mangle]
pub fn print_int(int: i64) {
    println!("print_int: {:#.?}", int);
}

#[used]
static EXTERNAL_FNS3: [fn(i64); 1] = [print_int];

// #[no_mangle]
// pub fn base_print(pj_str: PjStr) {
//     print_bytes(pj_str.buffer as *const u8, pj_str.length);
// }

// #[used]
// static EXTERNAL_FNS15: [fn(PjStr); 1] = [base_print];

use libc::{c_void, malloc};
// You can run this example from the root of the mio repo:
// cargo run --example tcp_server --features="os-poll net"
use mio::event::Event;
use mio::net::{TcpListener, TcpStream};
use mio::{Events, Interest, Poll, Registry, Token};
use std::collections::HashMap;
use std::io::{self, Read, Write};
use std::mem::size_of;

use crate::codegen::print_bytes;

// // Setup some tokens to allow us to identify which event is for which socket.
const SERVER: Token = Token(0);

// // Some data we'll send over the connection.
// const DATA: &[u8] = b"Hello world!\n";

#[used]
static EXTERNAL_FNS6: [extern "C" fn(&PjStr) -> *mut c_void; 1] = [pj_malloc_struct];

fn pjstr_to_str(pj_str: &PjStr) -> &str {
    unsafe {
        // Create a slice from the raw buffer and length
        let slice = core::slice::from_raw_parts(pj_str.buffer as *const u8, pj_str.length as usize);

        // Convert the slice to a UTF-8 str
        std::str::from_utf8(slice).unwrap()
    }
}

#[no_mangle]
pub extern "C" fn pj_malloc_struct(pj_name: &PjStr) -> *mut c_void {
    let name = pjstr_to_str(pj_name);

    match name {
        "TcpListener" => {
            let struct_size = size_of::<TcpListener>();
            let ptr = unsafe { malloc(struct_size as libc::size_t) as *mut c_void };

            // unsafe { std::ptr::write(ptr as *mut TcpListener, TcpListener::new().unwrap()) };
            ptr
        }
        "IoPoll" => {
            let struct_size = size_of::<Poll>();
            let ptr = unsafe { malloc(struct_size as libc::size_t) as *mut c_void };

            unsafe { std::ptr::write(ptr as *mut Poll, Poll::new().unwrap()) };
            ptr
        }
        "IoEvents" => {
            let struct_size = size_of::<Events>();
            let ptr = unsafe { malloc(struct_size as libc::size_t) as *mut c_void };

            unsafe { std::ptr::write(ptr as *mut Events, Events::with_capacity(128)) };
            ptr
        }
        "IoConnections" => {
            let struct_size = size_of::<HashMap<Token, TcpStream>>();
            let ptr = unsafe { malloc(struct_size as libc::size_t) as *mut c_void };

            unsafe { std::ptr::write(ptr as *mut HashMap<Token, TcpStream>, HashMap::new()) };
            ptr
        }
        _name => panic!("pj_malloc_struct given unknown struct type: {}", _name),
    }
}

#[used]
static EXTERNAL_FNS9: [extern "C" fn(&mut PjTcpServer); 1] = [pj_listen];

#[no_mangle]
pub extern "C" fn pj_listen(pj_tcp_server: &mut PjTcpServer) {
    let addr = unsafe {
        format!(
            "{}:{}",
            pjstr_to_str(pj_tcp_server.host.as_ref().unwrap()),
            pjstr_to_str(pj_tcp_server.port.as_ref().unwrap())
        )
    }
    .parse()
    .unwrap();
    let mut server = TcpListener::bind(addr).unwrap();

    // Register the server with poll we can receive events for it.
    unsafe { pj_tcp_server.poll.as_ref().unwrap() }
        .registry()
        .register(
            &mut server,
            Token(pj_tcp_server.conn_id as usize),
            Interest::READABLE,
        )
        .unwrap();

    pj_tcp_server.conn_id = pj_tcp_server.conn_id + 1;

    unsafe { std::ptr::write(pj_tcp_server.tcp_listener, server) };

    // Unique token for each incoming connection.

    println!("You can connect to the server using `nc`:");
    println!(" $ nc 127.0.0.1 9000");
    println!("You'll see our welcome message and anything you type will be printed here.");
}

#[used]
static EXTERNAL_FNS10: [extern "C" fn(&mut PjTcpServer); 1] = [pj_poll];

#[no_mangle]
pub extern "C" fn pj_poll(pj_tcp_server: &mut PjTcpServer) {
    let events = unsafe { pj_tcp_server.events.as_mut().unwrap() };
    let poll = unsafe { pj_tcp_server.poll.as_mut().unwrap() };

    if let Err(err) = poll.poll(events, None) {
        if interrupted(&err) {
            return;
        }
        println!("{:#?}", err);
        return;
        // return Err(err);
    }
}

#[used]
static EXTERNAL_FNS13: [extern "C" fn(fn()); 1] = [pj_ref_test];

#[no_mangle]
pub extern "C" fn pj_ref_test(tcp_writable: fn()) {
    tcp_writable()
}

#[used]
static EXTERNAL_FNS11: [extern "C" fn(&mut PjTcpServer, &PjTcpEvents); 1] = [pj_check_events];

#[no_mangle]
pub extern "C" fn pj_check_events(pj_tcp_server: &mut PjTcpServer, pj_tcp_events: &PjTcpEvents) {
    let events = unsafe { pj_tcp_server.events.as_ref().unwrap() };
    let poll = unsafe { pj_tcp_server.poll.as_ref().unwrap() };
    let server = unsafe { pj_tcp_server.tcp_listener.as_ref().unwrap() };
    let unique_token = pj_tcp_server.conn_id;
    let connections = unsafe { pj_tcp_server.connections.as_mut().unwrap() };

    for event in events.iter() {
        match event.token() {
            SERVER => loop {
                // Received an event for the TCP server socket, which
                // indicates we can accept an connection.
                let (mut connection, address) = match server.accept() {
                    Ok((connection, address)) => (connection, address),
                    Err(e) if e.kind() == io::ErrorKind::WouldBlock => {
                        // If we get a `WouldBlock` error we know our
                        // listener has no more incoming connections queued,
                        // so we can return to polling and wait for some
                        // more.
                        break;
                    }
                    Err(e) => {
                        // If it was any other kind of error, something went
                        // wrong and we terminate with an error.
                        println!("{:#?}", e);
                        return;
                        // return Err(e);
                    }
                };

                println!("Accepted connection from: {}", address);

                let token = next(&mut Token(unique_token as usize));
                poll.registry()
                    .register(
                        &mut connection,
                        token,
                        Interest::READABLE.add(Interest::WRITABLE),
                    )
                    .unwrap();

                connections.insert(token, connection);

                let pj_tcp_connection = PjTcpConnection {
                    server: pj_tcp_server,
                    tcp_stream: connections.get_mut(&token).unwrap(),
                    event,
                };
                (pj_tcp_events.tcp_writable_fn)(&pj_tcp_connection);
            },
            token => {
                // Maybe received an event for a TCP connection.
                let done = if let Some(connection) = connections.get_mut(&token) {
                    handle_connection_event(
                        pj_tcp_server,
                        pj_tcp_events,
                        poll.registry(),
                        connection,
                        event,
                    )
                    .unwrap()
                } else {
                    // Sporadic events happen, we can safely ignore them.
                    false
                };
                if done {
                    if let Some(mut connection) = connections.remove(&token) {
                        poll.registry().deregister(&mut connection).unwrap();
                    }
                }
            }
        }
    }
}

fn next(current: &mut Token) -> Token {
    let next = current.0;
    current.0 += 1;
    Token(next)
}

/// Returns `true` if the connection is done.
fn handle_connection_event(
    pj_tcp_server: &mut PjTcpServer,
    pj_tcp_events: &PjTcpEvents,
    registry: &Registry,
    connection: &mut TcpStream,
    event: &Event,
) -> io::Result<bool> {
    let pj_tcp_connection = PjTcpConnection {
        server: pj_tcp_server,
        tcp_stream: connection,
        event,
    };

    if event.is_writable() {
        (pj_tcp_events.tcp_writable_fn)(&pj_tcp_connection);
    }

    if event.is_readable() {
        let mut connection_closed = false;
        let mut received_data = vec![0; 4096];
        let mut bytes_read = 0;
        // We can (maybe) read from the connection.
        loop {
            match connection.read(&mut received_data[bytes_read..]) {
                Ok(0) => {
                    // Reading 0 bytes means the other side has closed the
                    // connection or is done writing, then so are we.
                    connection_closed = true;
                    break;
                }
                Ok(n) => {
                    bytes_read += n;
                    if bytes_read == received_data.len() {
                        received_data.resize(received_data.len() + 1024, 0);
                    }
                }
                // Would block "errors" are the OS's way of saying that the
                // connection is not actually ready to perform this I/O operation.
                Err(ref err) if would_block(err) => break,
                Err(ref err) if interrupted(err) => continue,
                // Other errors we'll consider fatal.
                Err(err) => return Err(err),
            }
        }

        if bytes_read != 0 {
            let received_data = &received_data[..bytes_read];
            let pj_str = PjStr {
                buffer: received_data.as_ptr() as *const i8,
                length: received_data.len() as i64,
                max_length: received_data.len() as i64,
            };

            (pj_tcp_events.tcp_data_received_fn)(&pj_tcp_connection, &pj_str)

            // if let Ok(str_buf) = from_utf8(received_data) {
            //     println!("Received data: {}", str_buf.trim_end());
            // } else {
            //     println!("Received (none UTF-8) data: {:.?}", received_data);
            // }
        }

        if connection_closed {
            println!("Connection closed");
            return Ok(true);
        }
    }

    Ok(false)
}

#[used]
static EXTERNAL_FNS12: [extern "C" fn(&mut PjTcpConnection, &PjStr); 1] = [pj_tcp_connection_write];

#[no_mangle]
pub extern "C" fn pj_tcp_connection_write(pj_tcp_connection: &mut PjTcpConnection, pj_str: &PjStr) {
    let connection = unsafe { pj_tcp_connection.tcp_stream.as_mut().unwrap() };
    let registry = unsafe {
        pj_tcp_connection
            .server
            .as_ref()
            .unwrap()
            .poll
            .as_ref()
            .unwrap()
    }
    .registry();
    let event = unsafe { pj_tcp_connection.event.as_ref().unwrap() };

    let slice = unsafe {
        // Create a slice from the raw buffer and length
        core::slice::from_raw_parts(pj_str.buffer as *const u8, pj_str.length as usize)
    };

    // We can (maybe) write to the connection.
    match connection.write(slice) {
        // We want to write the entire `DATA` buffer in a single go. If we
        // write less we'll return a short write error (same as
        // `io::Write::write_all` does).
        Ok(n) if n < slice.len() => {
            // return Err(io::ErrorKind::WriteZero.into())
            println!("{:#?}", io::ErrorKind::WriteZero);
            return;
        }
        Ok(_) => {
            // After we've written something we'll reregister the connection
            // to only respond to readable events.
            registry
                .reregister(connection, event.token(), Interest::READABLE)
                .unwrap()
        }
        // Would block "errors" are the OS's way of saying that the
        // connection is not actually ready to perform this I/O operation.
        Err(ref err) if would_block(err) => {}
        // Got interrupted (how rude!), we'll try again.
        Err(ref err) if interrupted(err) => {
            return pj_tcp_connection_write(pj_tcp_connection, pj_str)
        }
        // Other errors we'll consider fatal.
        // Err(err) => return Err(err),
        Err(err) => {
            println!("{:#?}", err);
            return;
        }
    }
}

fn would_block(err: &io::Error) -> bool {
    err.kind() == io::ErrorKind::WouldBlock
}

fn interrupted(err: &io::Error) -> bool {
    err.kind() == io::ErrorKind::Interrupted
}
