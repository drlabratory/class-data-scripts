require(tidyverse)

# A way to split a column into 
test_txt <- c("1234567890", "234567890", "4567890", "67890")
test_txt <- as.data.frame(test_txt)

#separate based on position, right index 
# The confusing part is it splits things in the order listed,
# splitting the first one, then the second one
# so sep = c(-6,-3) will give a different results than
# sep = c(-3, -6)
test_txt <- test_txt %>% 
  separate(col = test_txt,
           into = c("Left", "Middle", "Right"), 
           remove = FALSE, 
           sep = c(-6,-3))

test_txt <- test_txt %>% 
  separate(col = test_txt,
           into = c("Left_2", "Middle_2", "Right_2"), 
           remove = FALSE, 
           sep = c(3,6))