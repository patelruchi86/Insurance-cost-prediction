

########## 1 Import Libraries ##########

    suppressMessages(library(tidyverse))        # data manipulation and plots
    suppressMessages(library(funModeling))      # overview stats
    library(magrittr)                           # to use pipes
    library(skimr)                              # to get a quick summary table
    library(caret)                              # to create the partition for training/test datasets

    options(scipen = 999)                              # turn off scientific notation for numbers
    options(repr.plot.width=12, repr.plot.height=8)    # set universal plot size

########## 2 Import Dataset and pre-processing ##########

    # read the file in
    df <- read_csv('insurance.csv')

    # denote factor variables
    df$sex <- factor(df$sex)
    df$smoker <- factor(df$smoker)
    df$region <- factor(df$region)
    df$children <- factor(df$children)

    # check for missing values
    df %>%
      is.na() %>%
      sum()

    # check data types
    df %>%
      str()

########## 3 Exploratory data analysis ##########

### 3.1 overview ###

    #skim(df)
    summary(df)
    glimpse(df)

### 3.2 Distribution of Categorial variables (Smoker,Region,Sex,Children) ###

    figsize <- options(repr.plot.width=12, repr.plot.height=12) # set plot size for this plot 

    # Smoker count plot
    smoker <- df %>%
      ggplot(aes(x=smoker, fill=smoker)) + geom_bar(show.legend = FALSE) +
      # add percentages on top of bars
      geom_text(stat='count',aes(label=paste0(round(after_stat(prop*100), digits=1), "%"),group=1),vjust=-0.4, size=4 ) +
      # add labels
      labs(x = "",y = "",title = "Number of policyholders by smoking") +
      # rename x-ticks
      scale_x_discrete(labels = c("no" = "Non-smoker", "yes" = "Smoker") ) +
      # adjust y-ticks
      scale_y_continuous( breaks=seq(0,2000,100)) +
      # resize text
      theme( plot.title = element_text(size=16),axis.text.x = element_text(size=14),axis.text.y = element_text(size=14))
      #smoker

    # Region count plot
    region <- df %>%
      ggplot(aes(x=forcats::fct_infreq(region), fill=region)) + geom_bar(show.legend = FALSE) +
      # add percentages on top of bars
      geom_text(stat='count',aes(label = paste0(round(after_stat(prop*100), digits=1), "%"), group=1), vjust=-0.4, size=4 ) +
      # add labels
      labs( x = "",  y = "", title = "Number of policyholders by region") +
      # rename x-ticks
      scale_x_discrete(labels = c("northeast" = "North East", "northwest" = "North West",
                                 "southeast" = "South East", "southwest" = "South West") ) +
      # adjust ticks
      scale_y_continuous( breaks=seq(0,350,50)) +
      # resize text
      theme( plot.title = element_text(size=16),  axis.text.x = element_text(size=14),axis.text.y = element_text(size=14))
      #region

    # Sex count plot
    sex <- df %>%
     ggplot(aes(x=forcats::fct_infreq(sex), fill=sex)) +geom_bar(show.legend = FALSE) +
     # add percentages on top of bars
     geom_text(stat='count', aes( label=paste0(round(after_stat(prop*100), digits=1), "%"), group=1),vjust=-0.4,size=4) +
     # add labels
     labs(x = "",y = "",title = "Number of policyholders by sex",fill = "Sex") +
     # rename x-ticks
     scale_x_discrete(labels = c("male" = "Male", "female" = "Female")) +
     # adjust y-ticks
     scale_y_continuous(breaks=seq(0,700,100)) +
     # resize text
     theme(plot.title = element_text(size=16),axis.text.x = element_text(size=14),axis.text.y = element_text(size=14) ) 
     #sex

    # Children count plot
    children <- df %>%
     ggplot(aes(x=forcats::fct_infreq(children), fill=children)) + geom_bar(show.legend = FALSE) +
     # add percentages
     geom_text(stat='count', aes(label=paste0(round(after_stat(prop*100), digits=1), "%"), group=1), vjust=-0.4,size=4) +
     # add labels
     labs(x = "",y = "",title = "Number of dependents per policy") +
     # rename x-ticks
     scale_x_discrete(labels = c("0" = "None")) +
     # adjust y-ticks
     scale_y_continuous( breaks=seq(0,600,50)) +
     # resize text
     theme( plot.title = element_text(size=16),axis.text.x = element_text(size=14),axis.text.y = element_text(size=14))
     #children


    # Plot grid
    cowplot::plot_grid(smoker, region, sex, children,labels="AUTO",ncol = 2,nrow = 2 )

    options(figsize)

### 3.3 Distribution of continuous variable (Age, BMI, Charges) ###

    figsize <- options(repr.plot.width=20, repr.plot.height=16)

    # Age distribution
    
    age_hist <- df %>%
     ggplot(aes(x=age))+geom_histogram( binwidth = 5,show.legend = FALSE,fill="#ff5733")+
     labs( x = "Ages of policyholders",  y = "Number of policyholders", title = "Distribution of ages" )+
     # resize text
     theme(plot.title = element_text(size=16), axis.text = element_text(size=14), axis.title = element_text(size=14) )
     age_hist

    age_dens <- df %>%
      ggplot(aes(x=age)) +geom_density( alpha=.3, fill="#ff5733")+
      labs( x = "Ages of policyholders", y = "Probability density", title = "Distribution of ages" )+
      # resize text
      theme(plot.title = element_text(size=16), axis.text = element_text(size=14), axis.title = element_text(size=14))

    age_box <- df %>%
      ggplot(aes(y=age)) + geom_boxplot( alpha=.5, fill="#ff5733")+coord_flip() +
      theme( axis.text.y = element_blank(), axis.ticks.y = element_blank())+
      labs(y = "Ages of policyholders", x = "", title = "Distribution of ages" )+  
      # resize text
      theme( plot.title = element_text(size=16), axis.text = element_text(size=14), axis.title = element_text(size=14) )  
    
    # BMI distribution
     
    bmi_hist <- df %>%
      ggplot(aes(x=bmi))+geom_histogram( binwidth = 4, show.legend = FALSE, fill = "#55ab11" )+
      labs( x = "BMI scores of policyholders",y = "Number of policyholders",title = "Distribution of BMI scores")+
      # resize text
      theme(plot.title = element_text(size=16),axis.text = element_text(size=14), axis.title = element_text(size=14))

    bmi_dens <- df %>%
      ggplot(aes(x=bmi)) +geom_density( alpha=.3, fill="#55ab11" )+
      labs( x = "BMI scores of policyholders",y = "Probability density",title = "Distribution of BMI scores" )+
      # resize text
      theme(plot.title = element_text(size=16), axis.text = element_text(size=14), axis.title = element_text(size=14))

    bmi_box <- df %>%
      ggplot(aes(y=bmi)) +geom_boxplot(  alpha=.5,  fill="#55ab11")+coord_flip() +
      theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())+
      labs(y = "BMI scores of policyholders",x = "", title = "Distribution of BMI scores")+
      # resize text
      theme(plot.title = element_text(size=16),axis.text = element_text(size=14),axis.title = element_text(size=14))

    # Charges distribution
    
    charges_hist <- df %>%
      ggplot(aes(x=charges)) + geom_histogram(  binwidth = 2000,  show.legend = FALSE,fill = "#FFC300")+
      labs(x = "Charges to policyholders ($)",y = "Number of policyholders",title = "Distribution of medical charge")+
      # resize text
      theme(plot.title = element_text(size=16),axis.text = element_text(size=14),axis.title = element_text(size=14))

    charges_dens <- df %>%
      ggplot(aes(x=charges)) +geom_density( alpha=.3, fill="#FFC300" ) +
      labs( x = "Charges to policyholders ($)",y = "Probability density",title = "Distribution of medical charges") +
      # resize text
      theme( plot.title = element_text(size=16), axis.text = element_text(size=14), axis.title = element_text(size=14))

    charges_box <- df %>%
       ggplot(aes(y=charges))+geom_boxplot( alpha=.5,fill="#FFC300")+ coord_flip()+
       # remove ticks from y-axis
       theme( axis.text.y = element_blank(),axis.ticks.y = element_blank())+
       labs(y = "Charges to policyholders ($)",x = "",title = "Distribution of medical charges")+
       # resize text
       theme(plot.title = element_text(size=16),axis.text = element_text(size=14),axis.title = element_text(size=14))

    #grid Plot
    cowplot::plot_grid( age_hist, age_dens, age_box,bmi_hist, bmi_dens, bmi_box,charges_hist, charges_dens, charges_box,
                        labels="AUTO",ncol = 3,nrow = 3)
    options(figsize)

### 3.4 Distribution of Charges vs all other factors (smoke, region,sex,children) ###

    figsize <- options(repr.plot.width=20, repr.plot.height=26)

   # Boxplots
    
    chargesBysmoker <- df %>%
     ggplot( aes(x=forcats::fct_reorder(smoker, charges, .fun=median, .desc=TRUE),y=charges,fill=smoker)) +
     geom_boxplot(show.legend = FALSE) +coord_flip() +
     labs(x = "",y = "Charges to policyholders ($)",title = "Distribution of charges by smoking")+
     scale_x_discrete(labels = c("no" = "Non-smoker", "yes" = "Smoker"))+
     # resize text
     theme( plot.title = element_text(size=18), axis.text = element_text(size=14), axis.title = element_text(size=14))

    chargesByregion <- df %>%
     ggplot(aes(x=forcats::fct_reorder(region, charges, .fun=median, .desc=TRUE),y=charges,fill=region ) ) +
     geom_boxplot(show.legend = FALSE) +coord_flip() +
     labs(x = "",y = "Charges to policyholders ($)",title = "Distribution of charges by region"  )+
     scale_x_discrete( labels = c("northeast" = "North East", "northwest" = "North West",
                      "southeast" = "South East", "southwest" = "South West"))+
     # resize text
     theme(plot.title = element_text(size=18),axis.text = element_text(size=14),axis.title = element_text(size=14))

    chargesBychildren <- df %>%
     ggplot(aes(x=forcats::fct_reorder(children, charges, .fun=median, .desc=TRUE),y=charges,fill=children)) +
     geom_boxplot(show.legend = FALSE) +coord_flip() +
     labs( x = "", y = "Charges to policyholders ($)", title = "Distribution of charges by dependents")+
     scale_x_discrete(labels = c("0" = "None"))+
     # resize text
     theme(plot.title = element_text(size=18),axis.text = element_text(size=14),axis.title = element_text(size=14))

    chargesBysex <- df %>%
      ggplot(aes(x=forcats::fct_reorder(sex, charges, .fun=median, .desc=TRUE),y=charges,fill=sex) ) +
      geom_boxplot(show.legend = FALSE) + coord_flip() +
      labs(x = "",y = "Charges to policyholders ($)",title = "Distribution of charges by sex")+
      scale_x_discrete(labels = c("male" = "Male", "female" = "Female"))+
      # resize text
      theme(plot.title = element_text(size=18),axis.text = element_text(size=14),axis.title = element_text(size=14))
    
   # Density plots with medians
    
   densityBySmoker <- df %>%
     ggplot(aes( x=forcats::fct_reorder(smoker, charges, .fun=median, .desc=TRUE), y=charges, fill=smoker )) +
     geom_violin(show.legend = FALSE) +geom_boxplot(width=0.1,show.legend = FALSE)+coord_flip() +
     labs(x = "",y = "Charges to policyholders ($)",title = "Distribution of charges with density by smoking")+
     scale_x_discrete(labels = c("no" = "Non-smoker", "yes" = "Smoker") )+
     # resize text
     theme( plot.title = element_text(size=18),axis.text = element_text(size=14),axis.title = element_text(size=14))

   densityByRegion <- df %>%
     ggplot(aes(x=forcats::fct_reorder(region, charges, .fun=median, .desc=TRUE),y=charges,fill=region)) +
     geom_violin(show.legend = FALSE) +geom_boxplot(width=0.1,show.legend = FALSE)+    coord_flip() +
     labs(x = "",y = "Charges to policyholders ($)",title = "Distribution of charges with density by region")+
     scale_x_discrete(labels = c("northeast" = "North East", "northwest" = "North West",
                              "southeast" = "South East", "southwest" = "South West"))+
    # resize text
    theme(plot.title = element_text(size=18),axis.text = element_text(size=14),axis.title = element_text(size=14))

   densityBySex <- df %>%
     ggplot(aes(x=forcats::fct_reorder(sex, charges, .fun=median, .desc=TRUE),y=charges,fill=sex)) +
     geom_violin(show.legend = FALSE) +geom_boxplot(width=0.1,show.legend = FALSE)+    coord_flip() +
     labs(x = "",y = "Charges to policyholders ($)",title = "Distribution of charges with density by sex" )+
     scale_x_discrete(labels = c("male" = "Male", "female" = "Female"))+
     # resize text
     theme(plot.title = element_text(size=18),axis.text = element_text(size=14),axis.title = element_text(size=14))

   densityByChildren <- df %>%
     ggplot(aes(x=forcats::fct_reorder(children, charges, .fun=median, .desc=TRUE),y=charges,fill=children)) +
     geom_violin(show.legend = FALSE) +geom_boxplot(width=0.1,show.legend = FALSE)+    coord_flip() +
     labs(x = "",y = "Charges to policyholders ($)",title = "Distribution of charges with density by dependents")+
     scale_x_discrete(labels = c("0" = "None"))+
     # resize text
     theme(plot.title = element_text(size=18),axis.text = element_text(size=14),axis.title = element_text(size=14))
  
   # Plot grid of all plots
     cowplot::plot_grid(chargesBysmoker, densityBySmoker,chargesByregion, densityByRegion,chargesBysex, densityBySex,
                        chargesBychildren, densityByChildren,labels="AUTO",ncol = 2,nrow = 4)

     options(figsize)

### 3.5 Relation between medical charges to Age & BMI ###

    figsize <- options(repr.plot.width=12, repr.plot.height=8)

    age_scatter <- df %>%
      ggplot(aes(x=age, y=charges)) +geom_point()+
      # add a linear regression line
      geom_smooth(method='lm')+
      labs(x = "Ages of policyholders",y = "Charges to policyholders ($)",title = "Medical Charges vs Policyholder Age" )+
      # resize text
      theme(plot.title = element_text(size=18),axis.text = element_text(size=14),axis.title = element_text(size=14))

    bmi_scatter <- df %>%
      ggplot(aes(x=bmi, y=charges)) +geom_point()+
      # add a linear regression line
      geom_smooth(method='lm')+
      labs(x = "BMI scores of policyholders",y = "Charges to policyholders ($)",title = "Medical Charges vs Policyholder BMI")+
      # resize text
      theme(plot.title = element_text(size=18),axis.text = element_text(size=14),axis.title = element_text(size=14))

    cowplot::plot_grid(age_scatter, bmi_scatter,labels="AUTO",ncol = 2,nrow = 1)

    options(figsize)

### 3.6 Relation between medical charges to categorical data (Sex, smoke, children, region) ###

    figsize <- options(repr.plot.width=20, repr.plot.height=22)

   # by sex
   age_scatter_sex <- df %>%
     ggplot(aes(x=age, y=charges, color=sex)) +geom_point()+
     labs(x = "Ages of policyholders",y = "Charges to policyholders ($)",title = "Medical Charges vs Policyholder Age by Sex",
          color = "Sex:")+
     scale_color_hue(labels = c("male" = "Male", "female" = "Female"))+guides(fill=FALSE)+
     # resize text
     theme(plot.title = element_text(size=18),axis.text = element_text(size=14),axis.title = element_text(size=14) )

   bmi_scatter_sex <- df %>%
      ggplot(aes(x=bmi, y=charges, color=sex)) +geom_point()+
      labs( x = "BMI scores of policyholders",y = "Charges to policyholders ($)",title = "Medical Charges vs Policyholder BMI by Sex",
            color = "Sex:")+
      scale_color_hue(labels = c("male" = "Male", "female" = "Female"))+guides(fill=FALSE)+
      # resize text
      theme(plot.title = element_text(size=18),axis.text = element_text(size=14),axis.title = element_text(size=14))

   #by smoker
    age_scatter_smoker <- df %>%
       ggplot(aes(x=age, y=charges, color=smoker)) +geom_point()+
       labs(x = "Ages of policyholders",y = "Charges to policyholders ($)",title = "Medical Charges vs Policyholder Age by Smoking",
            color = "Smoker:")+
       scale_color_hue(labels = c("no" = "Non-smoker", "yes" = "Smoker"))+guides(fill=FALSE)+
       # resize text
       theme(plot.title = element_text(size=18),axis.text = element_text(size=14),axis.title = element_text(size=14) )

    bmi_scatter_smoker <- df %>%
        ggplot(aes(x=bmi, y=charges, color=smoker)) +geom_point()+
        labs(x = "BMI scores of policyholders",y = "Charges to policyholders ($)",
             title = "Medical Charges vs Policyholder BMI by Smoking",
             color = "Smoking:")+
        scale_color_hue(labels = c("no" = "Non-smoker", "yes" = "Smoker"))+ guides(fill=FALSE)+
       # resize text
       theme(plot.title = element_text(size=18),axis.text = element_text(size=14),axis.title = element_text(size=14))

    #by children
    age_scatter_kids <- df %>%
        ggplot(aes(x=age, y=charges, color=children)) +geom_point()+
        labs(x = "Ages of policyholders",y = "Charges to policyholders ($)",
              title = "Medical Charges vs Policyholder Age by Dependents",
        color = "Dependents:")+
        scale_color_hue(labels = c("no" = "Non-smoker", "yes" = "Smoker"))+guides(fill=FALSE)+
        # resize text
        theme(plot.title = element_text(size=18),axis.text = element_text(size=14),axis.title = element_text(size=14))

    bmi_scatter_kids <- df %>%
        ggplot(aes(x=bmi, y=charges, color=children)) +geom_point()+
        labs(x = "BMI scores of policyholders",y = "Charges to policyholders ($)",
             title = "Medical Charges vs Policyholder BMI by Dependents",color = "Dependents:")+
        scale_color_hue(labels = c("0" = "None"))+guides(fill=FALSE)+
        # resize text
        theme(plot.title = element_text(size=18),axis.text = element_text(size=14),axis.title = element_text(size=14))

     #by region
      age_scatter_region <- df %>%
        ggplot(aes(x=age, y=charges, color=region)) +geom_point()+
        labs( x = "Ages of policyholders",y = "Charges to policyholders ($)",
              title = "Medical Charges vs Policyholder Age by Region",color = "Regions:")+
        scale_color_hue(labels = c("northeast" = "North East", "northwest" = "North West",
              "southeast" = "South East", "southwest" = "South West"))+guides(fill=FALSE)+
        # resize text
        theme(plot.title = element_text(size=18),axis.text = element_text(size=14),axis.title = element_text(size=14))

      bmi_scatter_region <- df %>%
        ggplot(aes(x=bmi, y=charges, color=region)) +geom_point()+
        labs(x = "BMI scores of policyholders",y = "Charges to policyholders ($)",
             title = "Medical Charges vs Policyholder BMI by Regions",color = "Regions:")+
        scale_color_hue(labels = c("northeast" = "North East", "northwest" = "North West",
                             "southeast" = "South East", "southwest" = "South West"))+guides(fill=FALSE)+
        # resize text
        theme(plot.title = element_text(size=18),axis.text = element_text(size=14),axis.title = element_text(size=14))

    # make a grid
     cowplot::plot_grid(age_scatter_sex, bmi_scatter_sex,age_scatter_smoker, bmi_scatter_smoker,age_scatter_kids, bmi_scatter_kids,
                       age_scatter_region, bmi_scatter_region,labels="AUTO",ncol = 2,nrow = 4)

    options(figsize)

########## 4 Hypothesis testing ##########


### 4.1 Region vs charges ###

    df %>%
    group_by(region) %>%
    summarise(count = n(),min = min(charges),median = median(charges),max = max(charges),IQR = IQR(charges)) %>%
    arrange(desc(median)) # sort by median in descending order

    kruskal.test(charges ~ region, data = df)


### 4.2 BMI vs Gender ###
    
    df %>%
      group_by(sex) %>%
      summarise(count = n(),min = min(bmi),median = median(bmi),max = max(bmi),IQR = IQR(bmi)) %>%
      arrange(desc(median)) # sort by median in descending order
    
    #shapiro.test(df$sex)
    kruskal.test(bmi ~ sex, data = df)
    #pairwise.wilcox.test(df$bmi, df$sex, p.adj = "BH")
    
    
### 4.3 One Way ANOVA test: compare the mean of more than two independent groups ###
    
    # Example: is the bmi equal among regions?
    
    aovt<-aov(bmi ~ region,  data=df)
    summary(aovt)
    
    #Tukey pairwise comparison
    TukeyHSD(aovt) # pairwise comparison
    
    # checking ANOVA assumptions: homogenety of variances and normality of residuals
    options(repr.plot.width=6, repr.plot.height=6)
    plot(aovt, 1) # it doesnot seems to be a relationship here
    plot(aovt, 2) # I would assume normality with this plot
    
    shapiro.test(x=residuals(object = aovt)) # but here there is an indication of normality violated
    
    # non parametris option
    kruskal.test(bmi ~ region,  data=df)
    
    
########## 5 Multiple linear regression ##########

### 5.1 log transformation ###

    charges_hist <- df %>%
        ggplot(aes(x=charges)) + geom_histogram(binwidth = 2000,show.legend = FALSE,fill = "#FFC300")+
        labs( x = "Charges to policyholders ($)", y = "Number of policyholders", 
              title = "Distribution of medical charges")+
    # resize text
    theme(plot.title = element_text(size=16),axis.text = element_text(size=14),axis.title = element_text(size=14))

    charges_hist_log10 <- df %>%
      ggplot(aes(x=log10(charges)) ) +
      geom_histogram(show.legend = FALSE,fill = "#FFC300")+
      labs(x = "Charges to policyholders log10 transformed",y = "Number of policyholders",
          title = "Distribution of medical charges after log10 transform")+
      # resize text
      theme(plot.title = element_text(size=16),axis.text = element_text(size=14),
            axis.title = element_text(size=14))

      cowplot::plot_grid( charges_hist, charges_hist_log10,labels="AUTO",ncol = 2,nrow = 1)

      # log10 transform of response variable 
      df$logCharges<- log10(df$charges)
      
      
### 5.2 Split the dataset and train the model ###


      # Split the data into training and test sets
      set.seed(122)                    # Set the seed to make the partition reproducible
      training.samples <- df$logCharges %>%
      createDataPartition(p = 0.8, list = FALSE)
      train  <- df[training.samples, ]
      test <- df[-training.samples, ]

      # Train the model on the training dataset
      
      formula <- as.formula("logCharges ~ smoker + bmi + age + children + sex + region")
      model <- lm(formula, data = train)
      summary(model)

     # Make predictions on the training dataset
      predictions <- model %>% predict(train)
     # Model performance
     # (a) Calculating the residuals
     residuals <- train$logCharges - predictions
     # (b) Calculating Root Mean Squared Error
     rmse <- sqrt(mean(residuals^2))

     rmse %>%
     round(digits=3)

     # Make predictions on the testing dataset
     predictions <- model %>% predict(test)
     # Model performance
     # (a) Calculating the residuals
     residuals <- test$logCharges - predictions
     # (b) Calculating Root Mean Squared Error
     rmse <- sqrt(mean(residuals^2))

     rmse %>%
        round(digits=3)

    # Calculating RMSE for training data with backtransformed data

    predictions <- model %>% predict(train)
    # Model performance
    # (a) Calculating the residuals
    residuals <- 10^train$logCharges - 10^predictions # backtransform measured and predicted values
    # (b) Calculating Root Mean Squared Error
    rmse <- sqrt(mean(residuals^2))

    round(rmse)

    # Calculating RMSE for testing data with backtransformed data

    predictions <- model %>% predict(test)
    # Model performance
    # (a) Calculating the residuals
    residuals <- 10^test$logCharges - 10^predictions # backtransform measured and predicted values
    # (b) Calculating Root Mean Squared Error
    rmse <- sqrt(mean(residuals^2))

    round(rmse)

