module DropTree where 

import Model
import Item

tree :: Tree Item
tree = Node "Root" 
  [ 
    Node "Weapons" 
    [ 
      Leaf (axe Nothing),
      Leaf (sword Nothing)
    ],
    Node "Potions" 
    [
      Leaf (healthPotion Nothing)
    ]
  ]
