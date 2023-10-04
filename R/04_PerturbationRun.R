library(MortalitySmooth)
library(magic)
library(colorspace)
library(ggplot2)
library(plotly)
library(viridis)
library(Matrix)
library(tidyverse)
source("R/04_PerturbationFunction.R")

deaths <- read.csv("data_inter/deaths_sourced_infant_based_99.csv", header=TRUE)
## loading population
offset <- read.csv("data_inter/offsets_99.csv", header=TRUE)



# source("R/temp_function.r")
# for spot testing
# deaths.j <- 
#   deaths |> 
#   filter(Country=="Andorra" & Sex == "m") 
# fit_excess(offset = offset)

# "Andorra" computationally singular

# this is a last-ditch effort since
# error-trapping in harder in parallel.
# approx 30-50 minutes execution time
do_big_fit <- FALSE
if (do_big_fit){
big_L <-
  deaths |> 
  filter(Sex != "t") |> 
  group_by(Country, Sex) |> 
  group_split()

N     <- length(big_L)
out_L <- vector("list", N)
for (n in 1:N){
  ctr  <- big_L[[n]]$Country[1]
  sx   <- big_L[[n]]$Sex[1]
  outn <- try(fit_excess(big_L[[n]], offset = offset))
  if (class(outn)[1] == "try-error"){
    outn <- tibble(ages=integer(),
                   years=integer(),
                   up=double(),
                   low=double(),
                   type=character(),
                   value=double())
  }
  outn <-outn |> 
    mutate(Country = ctr,.before=1) |> 
    mutate(Sex = sx,.after=1)
  
  out_L[[n]] <- outn
}

big_test <- 
  out_L |> 
  bind_rows() |> 
  mutate(Sex = case_when(Sex == "m" ~ "male",
                         Sex == "f" ~ "female",
                         Sex == "t" ~ "total"))

big_test |> 
  write_csv("data_inter/eta_all_linear_time.csv")
}

# big_test <- read_csv("data_inter/eta_all.csv")
big_test <- read_csv("data_inter/eta_all_linear_time.csv")

big_test |> head()
big_test |> pull(type) |> unique()

mu <- big_test |> filter(Country == "Spain", 
                         type == "Obs Logrates", 
                         Sex == "female")
mu |> 
  ggplot(aes(x = ages, y = value,group = years)) +
  geom_line()
"Baseline Logrates"

spain3 <- 
  big_test |> 
  select(-up, -low) |> 
  filter(Country == "Spain", 
         type %in% c("Obs Offset","Obs Deaths","Baseline Logrates"),
         years == 2020) |> 
  pivot_wider(names_from = type, values_from = value) |>
  mutate(deaths_expected = exp(`Baseline Logrates`) * `Obs Offset`,
         deaths_excess = `Obs Deaths` - deaths_expected,
         Age = ages - ages %% 5) |> 
  group_by(Sex, Age) |> 
  summarize(`Excess Deaths` = sum(deaths_excess),
            .groups = "drop") |> 
  filter(Sex == "female") |> 
  ggplot(aes(x = Age+2.5, y = `Excess Deaths`)) +
  geom_col(width=5,alpha=.4) +
  theme_minimal() +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size = 16))+
  labs(x = "Age")
ggsave("Figures/spain3.pdf",spain3)

pull(type) |> unique()
b <- big_test |> filter(Country == "Spain", 
                        type == "Baseline Logrates", 
                        Sex == "female")

b |> 
  ggplot(aes(x = ages, y = value,group = years)) +
  geom_line()

spain1 <-
  mu |> 
  filter(years < 2020) |> 
  mutate(value = exp(value)) |> 
  ggplot(aes(x = ages, y = value,color = type)) +
  geom_point(color = "black", size = .5) +
  geom_line(data=b |> 
              mutate(value=exp(value))|> 
              filter(years < 2020), 
            color = "#E62600") +
  facet_wrap(~years) +
  theme_minimal() +
  scale_y_log10()+ 
  labs(y = "log mortality")
ggsave("Figures/spain1.pdf",spain1)

big_test$type |> unique()

big_test |> 
  filter(Country == "Spain",
         Sex == "female")

# Exp c
# Delta
expC <- big_test |> 
  filter(Country == "Spain",
         Sex == "female",
         years == 2020,
         type== "Exp c") |> 
  mutate(ages = 0)

spain2 <-
big_test |> 
  filter(Country == "Spain",
         Sex == "female",
         type == "Exp Delta",
         years == 2020) |> 
  ggplot(aes(x = ages,
             y = value,
             ymax = up,
             ymin = low)) +
  geom_line(color = rgb(1, .5,0), linewidth=2) + 
  geom_ribbon(alpha = .3, fill = rgb(1, .5,0)) +
  theme_minimal(base_size = 22) +
  geom_point(data = expC, color = rgb(0.54, 0.17, 0.89)) +
  geom_pointrange(data = expC, color = rgb(0.54, 0.17, 0.89)) +
  annotate("text",
           x = 4,
           y=expC$value, 
           label = "e^c",
           parse = TRUE, 
           size = 12, 
           color = rgb(0.54, 0.17, 0.89)) +
  annotate("text",
           x = 50,
           y=1.03, 
           label = "e^delta(x)",
           parse = TRUE, 
           size = 12,
           color = rgb(1, .5,0)) +
  labs(y="multiplicative factors",
       x = "Age") 
  # theme(axis.text=element_text(size=14))

ggsave("Figures/spain2.pdf",spain2)

big_test |> 
  filter(years %in% c(2020,2021),
         type == "Exp Delta",
         !Country %in% c("Andorra","Bermuda","Faroe Islands",
                         "Armenia","Azerbaijan","Nicaragua",
                         "Liechtenstein","Montserrat","Dominica"),
         ages== 25,
         value > 1.5)

deltas_all <-
big_test |> 
  filter(years %in% c(2020,2021),
         type == "Exp Delta",
         !Country %in% c("Andorra","Bermuda","Faroe Islands",
                         "Armenia","Nicaragua","Azerbaijan",
                         "Liechtenstein","Montserrat","Dominica",
                         "American Samoa","Luxembourg")) |> 
  ggplot(aes(x = ages, y = value, group = Country)) +
  geom_line(alpha=.3) +
  facet_wrap(Sex~years) +
  theme_minimal(base_size = 22) +
  ylim(.3,2) +
  labs(y =  expression(e^{delta}),
       x = "Age")

ggsave("Figures/deltas_all.pdf",deltas_all)

big_test |> 
  filter(years %in% c(2020,2021),
         type == "Exp Delta",
         Country %in% c("Jamaica")) |> 
  ggplot(aes(x = ages, y = value, group = Country)) +
  geom_line(alpha=.3) +
  facet_wrap(Sex~years) +
  theme_minimal(base_size = 22) +
  ylim(.3,2) +
  labs(y =  expression(e^{delta}),
       x = "Age")

deltas_LA1 <-
  big_test |> 
  filter(years %in% c(2020,2021),
         type == "Exp Delta",
         Country %in% c("Mexico","Peru","Guatemala",
                        "Brazil","Chile","Argentina",
                        "Colombia","Bolivia","Paraguay",
                        "Nicaragua","Ecuador","Panama",
                        "Dominican Republic")) |> 
  ggplot(aes(x = ages, y = value, group = Country)) +
  geom_line(alpha=.3) +
  facet_wrap(Sex~years) +
  theme_minimal(base_size = 22) +
  ylim(.3,2) +
  labs(y =  expression(e^{delta}),
       x = "Age")
deltas_LA1
ggsave("Figures/deltas_LA.pdf",deltas_LA1)


big_test |> 
  filter(years %in% c(2020,2021),
         type == "Exp Delta",
         Country %in% c("Cuba")) |> 
  ggplot(aes(x = ages, y = value, group = Country)) +
  geom_line(alpha=.3) +
  facet_wrap(Sex~years) +
  theme_minimal(base_size = 22) +
  ylim(.3,2) +
  labs(y =  expression(e^{delta}),
       x = "Age")


big_test |> 
  filter(years %in% c(2020,2021),
         type == "Exp Delta",
         Country %in% c("Mexico","Peru","Guatemala","Brazil","Colombia","Nicaragua","Ecuador","Panama")) |> 
  ggplot(aes(x = ages, y = value, group = Country)) +
  geom_line(alpha=.3) +
  facet_wrap(Sex~years) +
  theme_minimal(base_size = 22) +
  ylim(.3,2) +
  labs(y =  expression(e^{delta}),
       x = "Age")

deltas_excl_war <-
big_test |> 
  filter(years %in% c(2020,2021),
         type == "Exp Delta",
         Country %in% c("Armenia","Azerbaijan")) |> 
  ggplot(aes(x = ages, 
             y = value, 
             ymax = up, 
             ymin = low,
             color = Country, 
             fill = Country)) +
  geom_line() +
  geom_ribbon(alpha = .3) +
  facet_wrap(Sex~years) +
  theme_minimal(base_size = 22) +
  labs(y =  expression(e^{delta}),
       x = "Age")
ggsave("Figures/deltas_excl_war.pdf",deltas_excl_war)


low_signal <-
  big_test |> 
  filter(years %in% c(2020),
         type == "Exp Delta",
         Country %in% c("Dominica","Faroe Islands")) |> 
  ggplot(aes(x = ages, y = value, ymax = up, ymin=low, color = Sex)) +
  geom_line() +
  geom_ribbon(alpha=.3) +
  facet_wrap(~Country, scales = "free_y") +
  theme_minimal(base_size = 22) +
  labs(y =  expression(e^{delta}),
       x = "Age")
low_signal
ggsave("Figures/deltas_excl_low_signal.pdf",low_signal)
# -------------------------------------------------- #
# Following lines are for pdf flip-books of results
yr <- 2021
ctry <- big_test |> 
  filter(years == yr) |> 
  pull(Country) |> unique()

pdf(paste0("Figures/Fitted",yr,"all.pdf"))

for (cou.j in ctry){
  
  sexes <- big_test |> 
    filter(Country == cou.j,
           years == yr) |> 
    pull(Sex) |> 
    unique()
  for (sex in sexes){
    DF <-
      big_test |> 
      filter(Country == cou.j,
             Sex == sex,
             years == yr)
    
    DFobs <- DF |> 
      filter(type == "Obs Logrates") |> 
      mutate(ageup = ages + DemoTools::age2int(ages, OAvalue = 1)) |> 
      ungroup()
    
    p <-
      ggplot(DF, aes(x = ages, y = value, color = type)) +
      geom_segment(data = DFobs,
                   aes(x = ages, y = value, xend = ageup, yend = value), linewidth = 1) +
      geom_line(data = filter(DF, type == "Fitted Logrates"), linewidth = 1) +
      geom_ribbon(data = filter(DF, type == "Fitted Logrates"),
                  aes(ymin = low, ymax = up), alpha = .2) +
      geom_line(data = filter(DF, type == "Baseline Logrates"),
                aes(y = value), linewidth = 1.2) +
      labs(x = "age", y = "log-mortality", title = paste(cou.j,sex)) +
      theme_minimal()
    print(p)
  }
}

dev.off()

yr <- 2021
ctry <- big_test |> 
  filter(years == yr) |> 
  pull(Country) |> unique()

pdf(paste0("Figures/Deltas",yr,"all.pdf"))

for (cou.j in ctry){
  
  sexes <- big_test |> 
    filter(Country == cou.j,
           years == yr) |> 
    pull(Sex) |> 
    unique()
  for (sex in sexes){
    DFa <-
      big_test |> 
      filter(Country == cou.j,
             Sex == sex,
             years == yr,
             type %in% c("Exp c","Exp Delta"))
    
    .c  <-  DFa |> 
      filter(type == "Exp c") |> 
      mutate(ages=0)
    
    DFd <- DFa |> 
      filter(type == "Exp Delta") 
    
    p <-
      ggplot(DFa, aes(x = ages, y = value, color = type)) +
      geom_hline(yintercept = 1,
                 linewidth = .5,
                 color = gray(.5)) +
      geom_line(data = DFd, linewidth = 1) +
      geom_ribbon(
        data = DFd,
        mapping = aes(ymin = low, ymax = up),
        alpha = .2,
        color = "transparent",
        fill = "red"
      ) +
      geom_point(
        data = .c,
        mapping = aes(x = ages, y = value),
        color = "red",
        size = 2
      ) +
      geom_segment(
        data = .c,
        mapping = aes(
          x = ages,
          xend = ages,
          y = low,
          yend = up
        ),
        color = "red"
      ) +
      labs(x = "age",
           y = "age perturbation",
           title = paste(cou.j, sex)) +
      theme_minimal()
    print(p)
  }
}

dev.off()

# deprecated
# library(parallel)
# 
# fit_excess_wrap <- function(deaths.j, offset){
#   out <- try(fit_excess(deaths.j = deaths.j,
#                         offset = offset))
#   if (class(out) == "try-error"){
#     return(NULL)
#   } else {
#     return(out)
#   }
# }



check_losses <- FALSE
if (check_losses){
big_test <- read_csv("data_inter/eta_all.csv")

chunks_out <-
  big_test |> 
  select(Country, Sex) |> 
  distinct()

chunks_in <-
  deaths |> 
  select(Country, Sex) |> 
  distinct() |> 
  filter(Sex!="t") |> 
  mutate(Sex = ifelse(Sex == "m","male","female"))

chunks_lost <- 
  anti_join(chunks_in,chunks_out,by = join_by(Country, Sex))

# Country                        Sex
# ------------------------------------
# Andorra                        male
# Liechtenstein                  male
# Montserrat                     male
# Saint Vincent and Grenadines   female
# Saint Vincent and Grenadines   male

}