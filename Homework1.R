# Problem 1a

student_id <- c("S01", "S02", "S03", "S04", "S05", "S06")
section <- c("A", "B", "A", "B", "A", "B")
quiz1 <- c(82, 91, 76, 88, 95, 69)
quiz2 <- c(85, 89, 80, 92, 94, 74)
passed <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)

section <- factor(section, levels = c("A", "B"))

students <- data.frame(student_id, section, quiz1, quiz2, passed)

score_matrix <- matrix(c(quiz1, quiz2), nrow = 6, ncol = 2)
rownames(score_matrix) <- student_id
colnames(score_matrix) <- c("quiz1", "quiz2")

course_record <- list(
  course = "R Programming",
  scores = students,
  cutoffs = c(pass = 70, excellent = 90)
)

typeof(student_id)
class(student_id)
length(student_id)

str(section)
class(section)
length(section)

str(students)
dim(students)

str(score_matrix)
dim(score_matrix)

str(course_record)
length(course_record)

# A vector stores values of the same basic type. A list can store different types of objects.
# A matrix is a 2d structure with one type of data. A factor stores categorical data.
# A data frame stores data in rows and columns and can contain different types.


# Problem 1b

score_matrix["S04", "quiz2"]

score_matrix[1:2, , drop = FALSE]

course_record["course"]

course_record[["course"]]

course_record$course

# [ returns a subset of an object, [[ extracts a single element from a list, and $ takes out a named element from a list.


# Problem 1c

students$average <- rowMeans(students[, c("quiz1", "quiz2")])

students$excellent <- students$average >= 90

section_a <- students[students$section == "A" & students$average >= 80, ]

section_a[, c("student_id", "section", "average")]

student_averages <- students$average
names(student_averages) <- students$student_id

student_averages

# These calculations are vectorized because they operate on entire vectors at once.


# Problem 2

csv_text <- "sample_id,site,temp_c,ph,status
M01,North,18.2,7.1,ok
M02,South,20.5,,ok
M03,North,NA,6.8,review
M04,East,22.1,7.4,ok
M05,South,19.7,7.0,review
M06,East,23.0,NA,ok
M07,North,17.8,6.9,ok
M08,South,21.2,7.2,ok"

measurements <- read.csv(
  text = csv_text,
  na.strings = c("", "NA")
)

head(measurements)

str(measurements)

dim(measurements)

names(measurements)

colSums(is.na(measurements))

measurements_complete <- measurements[complete.cases(measurements), ]

measurements[!complete.cases(measurements), "sample_id"]

# M02, M03, and M06 were removed and x == NA is not a valid missing value test because comparing a value with NA produces NA.



# Problem 2b

measurements$site <- factor(measurements$site)

measurements$status <- factor(measurements$status)

levels(measurements$site)

levels(measurements$status)

measurements$temp_f <- measurements$temp_c * 9 / 5 + 32

measurements$ph_below_7 <- measurements$ph < 7

north_south_ok <- measurements[
  complete.cases(measurements) &
    measurements$site %in% c("North", "South") &
    measurements$status == "ok",
]

north_south_ok[, c("sample_id", "site", "temp_c", "temp_f", "ph")]

mean(measurements$temp_c, na.rm = TRUE)

mean(measurements$temp_c[measurements$site == "South"], na.rm = TRUE)

# The calculations use vectorized operations instead of a loop.


# Problem 2c

A <- matrix(1:4, nrow = 2)
B <- matrix(5:8, nrow = 2)

A * B

A %*% B

dim(A * B)

dim(A %*% B)

# Both results have dimensions 2 by 2 and * performs element multiplication, while %*% performs matrix multiplication.


# Problem 3

student_id <- paste0("P", sprintf("%02d", 1:8))
scores <- c(95, 82, NA, 67, 74, 88, 59, 91)


# Problem 3a

grade_one <- function(
    score,
    a_min = 90,
    b_min = 80,
    c_min = 70,
    d_min = 60
) {
  if (is.na(score)) {
    return(NA_character_)
  } else if (score >= a_min) {
    return("A")
  } else if (score >= b_min) {
    return("B")
  } else if (score >= c_min) {
    return("C")
  } else if (score >= d_min) {
    return("D")
  } else {
    return("F")
  }
}

grade_one(NA)
grade_one(90)
grade_one(80)
grade_one(85)
grade_one(74)


# Problem 3b

grades <- rep(NA_character_, length(scores))

for (i in seq_along(scores)) {
  grades[i] <- grade_one(scores[i])
}

names(grades) <- student_id

grades

# i represents the position of the current score in the scores vector.


# Problem 3c1

summarize_scores <- function(x, na.rm = TRUE, digits = 1) {
  total_count <- length(x)
  missing_count <- sum(is.na(x))
  mean_score <- mean(x, na.rm = na.rm)
  sd_score <- sd(x, na.rm = na.rm)
  min_score <- min(x, na.rm = na.rm)
  max_score <- max(x, na.rm = na.rm)
  
  c(
    total_count = total_count,
    missing_count = missing_count,
    mean = round(mean_score, digits),
    standard_deviation = round(sd_score, digits),
    minimum = round(min_score, digits),
    maximum = round(max_score, digits)
  )
}

summarize_scores(scores)

summarize_scores(
  x = scores,
  na.rm = TRUE,
  digits = 2
)


# Problem 3c2

plot_scores <- function(x, ...) {
  plot(seq_along(x), x, ...)
}

plot_scores(
  scores,
  type = "b",
  pch = 19,
  xlab = "Position",
  ylab = "Score",
  main = "Student Scores"
)

