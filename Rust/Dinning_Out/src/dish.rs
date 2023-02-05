pub struct Dish {
    pub id: i32,
    pub portions: i32,
    pub number_of_people: i32,
    pub people_that_tasted: Vec<i32>
}


impl Dish{

    pub fn new(id: i32, portions: i32, number_of_people: i32) -> Self{
        Dish{id: id, portions: portions, number_of_people: number_of_people, people_that_tasted: Vec::new()}
    }

    pub fn eat_portion(&mut self, person_id : i32) -> bool {
        if self.portions > 0{
            if self.can_eat(person_id){
                self.portions -= 1;
                let _ = &self.add_to_people_that_eated(person_id);
                return true;
            }
        }
        return false;
    }

    fn add_to_people_that_eated(&mut self, person_id : i32){
        self.people_that_tasted.push(person_id);
    }

    fn person_has_already_tasted(&self, person_id : i32) -> bool{
        for person_in_vector in &self.people_that_tasted{
            if person_in_vector == &person_id{
                return true;
            }
        }
        return false;
    }

    fn has_enough_portions(&self) -> bool{
        let people_that_tasted_clone : i32 =  self.people_that_tasted.len().try_into().unwrap();
        let number_of_people_clone = self.number_of_people.clone();

        let number_of_people_that_didnt_taste = number_of_people_clone - people_that_tasted_clone;
        return self.portions > number_of_people_that_didnt_taste;
    }

    fn can_eat(&self, person_id : i32) -> bool{
        return !&self.person_has_already_tasted(person_id) || self.has_enough_portions();
    }

}