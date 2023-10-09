; Create templates
(deftemplate Recipe
   (slot name)
   (slot cuisine)
   (slot difficulty)
   (slot ingredients)
   (slot instructions)
)

(deftemplate User
   (slot name)
   (slot cuisine-preference)
   (slot difficulty-preference)
   (slot ingredient-preference)
)

; Create Rules

(defrule GetUserPreferences
    (not (User (name ?name)))
    =>
    ; inputs for user
    (printout t "insert your name !")
    (bind ?name (read))
    (printout t "Welcome, " ?name ". Let's find you a recipe!" crlf)
    (printout t "What cuisine do you prefer? ")
    (bind ?cuisine-pref (read))
    (printout t "How difficult should the recipe be? (Easy, Intermediate, Difficult) ")
    (bind ?difficulty-pref (read))
    (printout t "Do you have any specific ingredient preferences? ")
    (bind ?ingredient-pref (read))
    ; create new user fact
    (assert (User 
        (name ?name)
        (difficulty-preference ?difficulty-pref)
        (cuisine-preference ?cuisine-pref)
        (ingredient-preference ?ingredient-pref)
    ))
)


(defrule RecommendRecipe
    (User 
        (name ?name)
        (cuisine-preference ?cuisine-pref)
        (difficulty-preference ?difficulty-pref)
        (ingredient-preference ?ingredient-pref)
    )
    (Recipe 
        (name ?recipe)
        (cuisine ?cuisine)
        (difficulty ?difficulty)
        (ingredients ?ingredients)
    )
    (test (eq ?cuisine ?cuisine-pref))
    (test (eq ?difficulty ?difficulty-pref))
    (test (str-compare ?ingredients ?ingredient-pref))
    =>
    (printout t "Based on your preferences, we recommend: " ?recipe crlf)
    (printout t "Ingredients: " ?ingredients crlf)
    (printout t "Enjoy your meal!" crlf)
    ;(retract (User (name ?name)))
    ;(retract (Recipe (name ?recipe)))
)

(defrule ExitRecommendation
   (User (name ?name))
   =>
   (printout t "Thank you for using the personalized recipe recommender, " ?name "!" crlf)
   ;(retract (User (name ?name)))
   ;(exit)
)

; set initial facts

(deffacts SampleRecipes
    (Recipe 
        (name "Spaghetti Carbonara") 
        (cuisine Italian) 
        (difficulty Easy)
        (ingredients "spaghetti, eggs, bacon, Parmesan cheese") 
        (instructions "1. Cook spaghetti. 2. Fry bacon. 3. Mix eggs and cheese. 4. Toss with pasta.")
    )

    (Recipe 
        (name "Chicken Tikka Masala") 
        (cuisine Indian) 
        (difficulty Intermediate)
        (ingredients "chicken, yogurt, tomato sauce, spices") 
        (instructions "1. Marinate chicken. 2. Cook chicken. 3. Simmer in sauce.")
    )

    (Recipe 
        (name "Caesar Salad") 
        (cuisine American) 
        (difficulty Easy)
        (ingredients "romaine lettuce, croutons, Caesar dressing") 
        (instructions "1. Toss lettuce with dressing. 2. Add croutons.")
    )

    (Recipe 
        (name "Sushi Rolls") 
        (cuisine Japanese) 
        (difficulty Intermediate)
        (ingredients "sushi rice, nori seaweed, fish, vegetables") 
        (instructions "1. Prepare sushi rice. 2. Lay out nori seaweed. 3. Add fillings and roll.")
    )

    (Recipe 
        (name "Mediterranean Quinoa Salad") 
        (cuisine Mediterranean) 
        (difficulty Easy)
        (ingredients "quinoa, cucumber, tomatoes, olives, feta cheese") 
        (instructions "1. Cook quinoa. 2. Dice vegetables. 3. Mix with quinoa and feta cheese.")
    )

    (Recipe 
        (name "Beef Stir-Fry") 
        (cuisine Chinese) 
        (difficulty Intermediate)
        (ingredients "beef, broccoli, bell peppers, soy sauce") 
        (instructions "1. Slice beef. 2. Stir-fry with vegetables and soy sauce.")
    )

    (Recipe 
        (name "Classic Margherita Pizza") 
        (cuisine Italian) 
        (difficulty Easy)
        (ingredients "pizza dough, tomatoes, fresh mozzarella, basil") 
        (instructions "1. Roll out pizza dough. 2. Add tomatoes, mozzarella, and basil. 3. Bake.")
    )
)