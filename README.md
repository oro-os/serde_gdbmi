# Serde deserializer for the GDB Machine Interface (GDB/MI)

This crate implements a [serde](https://serde.rs/) deserializer for
GDB Machine Interface (GDB/MI) output. It allows you to easily parse
GDB/MI output into Rust data structures using Serde's powerful
serialization framework.

This library does not specify the message types themselves, but rather
_just_ the deserialization logic. You can define your own Rust structs
that match the structure of the GDB/MI output you expect to receive, and
then use this crate to deserialize the GDB/MI output into those structs.

# Example

```rust
use serde::Deserialize;
use serde_gdbmi::{from_str, Response, ResponseBody};

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
struct GdbMiFrame {
    addr: String,
    func: String,
    arch: String,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
enum GdbMiOutput {
    // variant names become class names from the GDB/MI output
    ThreadGroupAdded {
        id: String,
    },
    Stopped {
        reason: String,
        signal_name: Option<String>,
        signal_meaning: Option<String>,
        frame: Option<GdbMiFrame>,
        thread_id: Option<String>,
        stopped_threads: String
    }
}

let gdb_mi_output = r#"^thread-group-added,id="i1""#;
let response: Response<GdbMiOutput> = from_str(gdb_mi_output).unwrap();
println!("{:#?}", response);
/*
    Response {
        token: None,
        body: ResponseBody::Result(
            GdbMiOutput::ThreadGroupAdded {
                id: "i1",
            },
        ),
    }
*/
```

# License
Copyright &copy; 2026 Josh Junon. Part of the Oro Operating System project.
Released under the MIT license.