use axum::{routing::post, Router};
use frostbite_compiler::{codegen::CodegenBackends, Compiler};
use tokio::net::TcpListener;
use tower_http::services::ServeDir;

const SERVER_ADDRESS: &str = "0.0.0.0:63219";

#[tokio::main]
async fn main()
{
    tracing_subscriber::fmt::init();

    let app = Router::new()
        .route("/compile", post(compile_source_code))
        .nest_service("/", ServeDir::new("./frontend"));

    let listener = TcpListener::bind(SERVER_ADDRESS).await.unwrap();

    tracing::info!("Server running on at '{SERVER_ADDRESS}'");

    axum::serve(listener, app).await.unwrap();
}

async fn compile_source_code(body: String)
{
    tokio::task::spawn_blocking(move || {
        let mut compiler = Compiler::new();

        let src_id = compiler.add_source("anonymous-file".to_string(), body);

        let _ = compiler
            .compile_source_code(src_id, CodegenBackends::bytecode_backend())
            .map_err(|_| compiler.move_ctx());
    })
    .await
    .unwrap();
}
