## techniques to avoid NOTES: no visible binding for global variable
## when doing R CMD CHECK for functions using package ggplot2

utils::globalVariables(c("x", "y", "comp", "..density.."))

