# ============================================================
# WEEK 8 — Student Dataset Analysis in R
# Dataset: student_data.csv (50 students, 5 columns)
# Columns: student_id, student_name, gender, coursework_mark, final_exam_mark
# ============================================================

# ---- Q5: How to obtain the dataset details using R ----------
# read the CSV file into a data frame
df <- read.csv("student_data.csv", stringsAsFactors = FALSE)

# view the structure (column names, types, sample values)
str(df)

# view the first few rows
head(df)

# summary statistics for each column
summary(df)


# ---- Q1: Students with final_exam_mark > 80% -----------------
high_scorers <- subset(df, final_exam_mark > 80)
print(high_scorers)


# ---- Q2: Arrange dataset in ascending order by coursework_mark
df_sorted <- df[order(df$coursework_mark), ]
print(df_sorted)


# ---- Q3: Display only student name and final_exam_mark -------
name_and_exam <- df[, c("student_name", "final_exam_mark")]
print(name_and_exam)


# ---- Q4: Number of rows and columns ---------------------------
dim(df)                 # gives rows, columns together: 50 6
nrow(df)                # rows only: 50
ncol(df)                # columns only: 5


# ---- Q6: Detect outliers using R -------------------------------
# Method: Boxplot (visual) + IQR rule (numeric)

# Visual method
boxplot(df$final_exam_mark,
        main = "Boxplot of Final Exam Marks",
        ylab = "Marks")

# Numeric method — Interquartile Range (IQR) rule
Q1 <- quantile(df$final_exam_mark, 0.25)
Q3 <- quantile(df$final_exam_mark, 0.75)
IQR_val <- IQR(df$final_exam_mark)

lower_bound <- Q1 - 1.5 * IQR_val
upper_bound <- Q3 + 1.5 * IQR_val

outliers <- subset(df, final_exam_mark < lower_bound | final_exam_mark > upper_bound)
print(outliers)   # for this dataset, this returns 0 rows (no outliers)
