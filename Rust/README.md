# project---dinning-out-team53607_61726
project---dinning-out-team53607_61726 created by GitHub Classroom

# mini-project-team53607_61726
mini-project-team53607_61726 created by GitHub Classroom

Ricardo Barqueira, 53607
Luis Almas, 61726

Relatório do Projeto "Dinning-out" de Rust

In this project we were presented with a problem where we have a group of friends that go for a dinner and each of them order a dish and they are supposed to try
every dish in the table. 

To implement this we created two different data structures: a dish list and the dish struct.

The dish list is composed by the head of the list, and each node is composed of a value
and the node next which represents the following node in the list. The list also implements the push function of a regular singly linked list and the iterator which iterates over 
the list and returns the vector with the references of the nodes values.


        type Link<T> = Option<Box<Node<T>>>;

        struct Node<T> {
            val: T,
            next: Link<T>
        }

        pub struct DishNodeList<T> {
            head: Link<T>
        }


The dish struct is a data structure that represents a dish in the dinner table and is composed of an id, the number of portions of the dish, the number of people at the table and
the list of people that tasted the dish. The node value of the nodes in the dish list is represented by the dish. The dish also implements a function that represents the portion
being eaten and some functions that verify if it's possible to eat (the function returns a boolean that tells if the person can eat the portion or not).

        pub struct Dish {
            pub id: i32,
            pub portions: i32,
            pub number_of_people: i32,
            pub people_that_tasted: Vec<i32>
        }

To represent the people eating simultaneously we spawned threads equal to the number of people. The dish list will be shared between the threads (Arc) and will allow for simultaneous
reads (RWLock). Each person will have a clone of the dish list that will perform the function iterate and receive a vector with every dish. This vector will be iterated over all the
dishes, starting with the corresponding dish (which are shared by an Arc and allow for only one person to access that dish in case of a write (RWLock)) and for each person will 
attempt to eat that dish. We will also be checking for dishes whose portions have reached zero and add them to a shared list in order to check if all dishes are empty. In the case
that not all dishes are empty, once the person has tried all dishes the vector will start to iterate again from the beginning. To keep track of how many portions have been eaten by
each person, we have a list with size equal to the number of dishes in which each index of the list corresponds to the id of the dish and the value for each position of the list is a
counter of the portions eaten by that person (this will be displayed in the end of the running program). 

            let dish_list = DishNode::DishNodeList::new();

            let share_dish_list = Arc::new(RwLock::new(add_dishes_to_list(
                dish_list,
                arguments.number_of_people,
                arguments.portions,
            )));

            let empty_dishes = Arc::new(RwLock::new(Vec::<i32>::new()));

            let mut thread_join_handles = Vec::<thread::JoinHandle<()>>::new();

 
 We can conclude the program satisfies the envisaged properties: Each thread runs simultaneously and will have its turn to taste the dish eventually because threads are
 iterating over a vector and after finishing an attempt to eat a portion they will release the write lock and proceed to the next dish in the list while letting others have access to 
 it, that also means that all threads are running without waiting for others meaning there's no risk of deadlock (Starvation Freedom and Deadlock Freedom), because we are locking the 
 data with a RWLock we ensure no other thread will overwrite the data written by that thread (No dataraces and Mutual Exclusion).


