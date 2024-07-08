use nvmber::Nvmber;

mod nvmber;

fn main() {
    let four = Nvmber::from("IV").unwrap();
    let three = Nvmber::from("III").unwrap();
    let two = Nvmber::from("II").unwrap();
    println!("{} + {} = {}", three, two, &three + &two);
    println!("{} + {} = {}", three, four, &three + &four);
    println!(
        "{} + {} + {} = {}",
        four,
        three,
        four,
        &four + &three + &four
    );
    let twofifty = Nvmber::from("CCL").unwrap();
    let fourtwentyfive = Nvmber::from("CDXXV").unwrap();
    println!(
        "{} + {} = {}",
        twofifty,
        fourtwentyfive,
        &twofifty + &fourtwentyfive
    );
    println!(
        "{} + {} - {} = {}",
        twofifty,
        fourtwentyfive,
        two,
        &twofifty + &fourtwentyfive - &two
    );
}
