## EDA
source("init.R")
ft_processed = import_ft() %>% process_ft()

remedy = ft_processed$remedy
ct = ft_processed$ct


ct_student = ct %>% group_by(datazone) %>% summarize(student_count = sum(known_student)) %>% mutate(students_present = student_count > 0)
tip_count = remedy %>% group_by(datazone) %>% summarize(tip_count = n()) 

tip_students = left_join(tip_count, ct_student, by="datazone")

tip_students = tip_students %>% mutate(student_count = ifelse(is.na(student_count), 0, student_count)) %>% data.frame()

tip_students %>% group_by(students_present) %>% summarize(tip_count = sum(tip_count), total = n()) %>% mutate(prop = tip_count/total) 

write.csv(tip_students %>% mutate(datazone = toupper(datazone)), '../data/export/tip_count_students.csv', row.names=F)

## model for estimating students impact on fly tipping
student_fit = glm(tip_count ~ student_count, data=tip_students, family = "gaussian")

### Check correlation
cor(tip_students[, c("tip_count", "student_count")])
