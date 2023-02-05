// A STACK OF INTEGERS AS A SINGLY LINKED LIST

// From
// https://rust-unofficial.github.io/too-many-lists/

type Link<T> = Option<Box<Node<T>>>;
// without Box Rust says: recursive type has infinite size
/* Box: a pointer type for heap allocation;
    provides ownership for this allocation, and drop their contents when they go out of scope.
    Essential for recursive data-structures: they must be boxed, because if the definition of Cons looked like
    Cons(T, List<T>),
    the size of a List depends on how many elements are in the list, and so we don't know how much memory to allocate for a Cons. By introducing a Box, which has a defined size, we know how big Cons needs to be.
 */

struct Node<T> {
    val: T,
    next: Link<T>
}

pub struct StkIList<T> {
    head: Link<T>
}

impl<T> StkIList<T> {
    pub fn new() -> Self {
        StkIList {head: None}
    }

    pub fn push(&mut self, val: T) {
        // make node -- steal a value out of a borrow by replacing it with another value
        let new_node = Box::new(Node {
            val: val,
            next: self.head.take()
        });
        self.head = Some(new_node);
    }

    /* Check if the list is empty. If it's empty, just return None
       If it's not empty, remove the head of the list, remove its elem, replace the list's head with its next
     */
    pub fn pop(&mut self) -> Option<T> {
        match self.head.take() {
            None => None,
            Some(node) => {
                self.head = node.next;
                Some(node.val)
            }
        }
    }

    /* match option { None => None, Some(x) => Some(y) } is so common that it was called map:
       it takes a function to execute on the x in the Some(x) to produce the y in Some(y).
       Let's use a closure -- an anonymous functions that they can refer to local variables outside its scope!
     */
    pub fn top(&self) -> Option<&T> {
        self.head.as_ref().map(|node| {
            &node.val
        })
    }
}

fn main() {
    let mut list = StkIList::new();

    // Check empty list behaves right
    println!("T1: top of empty - {:?}", list.top());
    println!("T2: pop from empty - {:?}", list.pop());

    // Populate list
    list.push(42);
    println!("Inserts 42");
    println!("T3: top of not empty - {:?}", list.top());
    println!("T4: pop from not empty - {:?}", list.pop());
    println!("T5: now is empty (top gives None) - {:?}", list.top());

    // Push some more just to make sure nothing's corrupted
    list.push(41);
    println!("Inserts 41");
    println!("T6: top of not empty - {:?}", list.top());
    list.push(42);
    println!("Inserts 42");
    println!("T7: top of not empty - {:?}", list.top());

    // Check normal removal
    println!("T8: pop gives 42 - {:?}", list.pop());
    println!("T9: pop gives 41 - {:?}", list.pop());

    // Check exhaustion
    println!("T7: pop gives None - {:?}", list.pop());
}