module Types.Roles where

type Roles = [Role]

data Role = Doctor | Invesitigator | Zealot | Lookout deriving (Show, Eq, Enum)

main = do
  let role = Doctor
  print role
  print $ fromEnum role
  print $ fromEnum Lookout
  print [Doctor ..]