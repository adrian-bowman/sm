#     Tests for the sm.options function

sm.options(describe = TRUE)
print(sm.options()$describe)
print(sm.options(describe = FALSE)$describe)
