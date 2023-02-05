type Link<T> = Option<Box<Node<T>>>;

struct Node<T> {
    val: T,
    next: Link<T>
}

pub struct DishNodeList<T> {
    head: Link<T>
}

impl<T> DishNodeList<T> {
    pub fn new() -> Self {
        DishNodeList {head: None}
    }

    pub fn push(&mut self, val: T) {
        // make node -- steal a value out of a borrow by replacing it with another value
        let new_node = Box::new(Node {
            val: val,
            next: self.head.take()
        });
        self.head = Some(new_node);
    }

    pub fn iterate(&self) -> Vec<&T> {
        let mut vec = Vec::new();
        match self.head.as_ref(){
            Some(head) => {
                vec.push(&head.val);
                let mut first_next_node = &head.next;
                loop{
                    match first_next_node.as_ref(){
                        Some(next_ref) => {
                            vec.push(&next_ref.val);
                            first_next_node = &next_ref.next;
                        },
                        None => {return vec }
                    }
                }
            },
            None => { return vec }
        }
    }
}