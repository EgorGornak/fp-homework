import Block1

main :: IO ()
main = do
  let x = Monday
  if isWeekend x
    then undefined
    else print ""
  let y = Saturday
  if not (isWeekend y)
    then undefined
    else print ""
