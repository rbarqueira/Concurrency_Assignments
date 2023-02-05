mod DishNode;
mod dish;

use crate::dish::Dish;
use std::env;
use std::sync::{Arc, RwLock};
use std::thread;
use std::time::Duration;

struct Arguments {
    number_of_people: i32,
    portions: i32,
}

fn get_arguments(args: Vec<String>) -> Option<Arguments> {
    if args.len() < 3 {
        println!("Missing parameters... (Number of people: Portions per dish:)");
        return None;
    }

    let number_of_people_arg = args[1].clone();
    let portion_arg = args[2].clone();

    let number_of_people = convert_arguments(&number_of_people_arg);
    let portions = convert_arguments(&portion_arg);

    if number_of_people == -1 || portions == -1 {
        println!("Invalid parameters...");
        return None;
    }

    if !portions_are_valid(&number_of_people, &portions) {
        println!("Invalid portions...");
        return None;
    }

    return Some(Arguments {
        number_of_people: number_of_people,
        portions: portions,
    });
}

fn convert_arguments(value: &String) -> i32 {
    return match value.parse() {
        Ok(n) => n,
        Err(_) => {
            eprintln!("error: argument not an integer");
            return -1;
        }
    };
}

fn add_dishes_to_list(
    dish_list: DishNode::DishNodeList<Arc<RwLock<Dish>>>,
    number_of_people: i32,
    portions: i32,
) -> DishNode::DishNodeList<Arc<RwLock<Dish>>> {
    let mut dish_list_to_insert = dish_list;
    for i in 0..number_of_people {
        dish_list_to_insert.push(Arc::new(RwLock::new(Dish::new(
            i,
            portions,
            number_of_people,
        ))));
    }

    return dish_list_to_insert;
}

fn portions_are_valid(number_of_people: &i32, portions: &i32) -> bool {
    number_of_people <= portions
}

fn no_portions_left(number_of_people: i32, empty_dishes: Vec<i32>) -> bool {
    let num_usize: usize = number_of_people as usize;
    empty_dishes.len() == num_usize
}

fn can_to_empty_dishes_list(empty_dish_list: &Vec<i32>, dish_id: i32) -> bool {
    for id in empty_dish_list.iter() {
        if *id == dish_id {
            return false;
        }
    }
    return true;
}

fn create_vec_with_zero(number_of_people: usize) -> Vec<i32> {
    return vec![0; number_of_people];
}

fn get_tasted_dishes(tasted_dishes: Vec<i32>) -> String {
    let mut string: String = format!("Dish:0 Portions:{}", tasted_dishes[0]);

    for i in 1..tasted_dishes.len() {
        let dish_string: String = format!("Dish:{} Portions:{}", i, tasted_dishes[i]);
        if i == tasted_dishes.len() {
            string = format!("{} {}", string, dish_string);
            break;
        }
        string = format!("{}, {}", string, dish_string);
    }

    return format!("[ {} ]", string);
}

fn wait_for_threads(thread_handles_vec: Vec<thread::JoinHandle<()>>) {
    for handle in thread_handles_vec {
        handle.join().unwrap();
    }
}

fn person_eating(
    vec: Vec<&std::sync::Arc<std::sync::RwLock<dish::Dish>>>,
    thread_index: i32,
    mut i: usize,
    tasted_dishes_vec: &mut Vec<i32>,
    empty_dishes_clone: Arc<RwLock<Vec<i32>>>,
) {
    while i != vec.len() {
        if let Ok(mut write_guard) = vec[i].write() {
            if write_guard.eat_portion(thread_index) {
                println!("Person {} ate from Dish {}", thread_index, write_guard.id);
                let dish_vec_index: usize = write_guard.id.clone().try_into().unwrap();
                tasted_dishes_vec[dish_vec_index] += 1;
                thread::sleep(Duration::from_millis(1000));
            }
        }

        if vec[i].read().unwrap().portions == 0 {
            if let Ok(mut write_guard2) = empty_dishes_clone.write() {
                if can_to_empty_dishes_list(&write_guard2, vec[i].read().unwrap().id) {
                    write_guard2.push(vec[i].read().unwrap().id);
                }
            }
        }
        i += 1;
        let read_empty_dishes = empty_dishes_clone.read().unwrap();
        if no_portions_left(
            vec[i - 1].read().unwrap().number_of_people,
            read_empty_dishes.to_vec(),
        ) {
            break;
        }

        if i == vec.len() {
            i = 0;
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let arguments = get_arguments(args);

    match arguments {
        Some(arguments) => {
            let dish_list = DishNode::DishNodeList::new();

            let share_dish_list = Arc::new(RwLock::new(add_dishes_to_list(
                dish_list,
                arguments.number_of_people,
                arguments.portions,
            )));

            let empty_dishes = Arc::new(RwLock::new(Vec::<i32>::new()));

            let mut thread_join_handles = Vec::<thread::JoinHandle<()>>::new();

            for people_index in 0..arguments.number_of_people {
                let dish_list_clone = share_dish_list.clone();
                let empty_dishes_clone = empty_dishes.clone();

                let join = std::thread::spawn(move || {
                    let list = dish_list_clone.read().unwrap();
                    let vec = list.iterate();
                    let thread_index = people_index;
                    let mut tasted_dishes_vec: Vec<i32> = create_vec_with_zero(
                        arguments.number_of_people.clone().try_into().unwrap(),
                    );

                    let i: usize = thread_index.clone().try_into().unwrap();

                    person_eating(
                        vec,
                        thread_index,
                        i,
                        &mut tasted_dishes_vec,
                        empty_dishes_clone,
                    );
                    println!(
                        "Person {} is done, tasted dishes: {}",
                        thread_index,
                        get_tasted_dishes(tasted_dishes_vec)
                    );
                });

                thread_join_handles.push(join);
            }

            wait_for_threads(thread_join_handles);
        }
        None => return,
    }
}
