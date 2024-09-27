use crossbeam_channel::{unbounded, Receiver};
use std::thread;
use std::io;

pub fn spawn_task_with_cancellation<F>(task: F) -> Result<(), Box<dyn std::error::Error>>
where
    F: FnOnce(Receiver<()>) -> Result<(), Box<dyn std::error::Error>> + Send + 'static,
{
    // Create a crossbeam channel for cancellation
    let (cancel_sender, cancel_receiver) = unbounded();

    // Spawn a thread for a long-running task
    let handle = thread::spawn(move || {
        task(cancel_receiver).expect("Receiver failed");
    });

    // Wait for user input (key press to cancel)
    println!("Press Enter to cancel the task...");
    let _ = io::stdin().read_line(&mut String::new())?;

    // Send a cancellation signal
    cancel_sender.send(())?;

    // Wait for the task to finish
    handle.join().expect("TODO: panic message");

    Ok(())
}
