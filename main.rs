fn main() {
    println!("Hi there");
}

fn halve_to_one(x: u32) -> impl Iterator<u32> {
    if x = 1 {
        vec![1]
    } else {
        ... something here like "[x] + halve_to_one(x/2)" ...
    }
} 