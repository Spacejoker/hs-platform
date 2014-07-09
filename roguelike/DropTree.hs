module DropTree where 

import Model
import Item

tree :: Tree Item
tree = Node "Root" 
  [ 
    Node "Weapons" 
    [ 
      Leaf axe,
      Leaf sword
    ],
    Node "Armours" 
    [
      Leaf tunic,
      Leaf boots
    ]
  ]
