library(pivottabler)
### doc : http://www.pivottabler.org.uk/articles/v04-regularlayout.html

# copy filtered data as data.frame
data = as.data.frame(number_pupae_tested)

# format species name
short_name = data$Species
short_name = gsub("_", "", short_name, fixed = T)
short_name = gsub("Drosophila", "D.", short_name, fixed = T)
short_name = gsub("Megaselia", "M.", short_name, fixed = T)
short_name = gsub("Scaptodrosophila", "S.", short_name, fixed = T)
short_name = gsub("Zaprionus", "Z.", short_name, fixed = T)
data$Species = short_name

# format comment name
temp_comment = data$Comment
temp_comment = gsub("^cuticle_broke$", "Cuticle broke", temp_comment)
temp_comment = gsub("^not_detached$", "Not detached", temp_comment)
temp_comment = gsub("^ok$", "Detached", temp_comment)
data$Comment = temp_comment

data$Species = as.factor(data$Species)
data$Protocol = as.factor(data$Protocol)
data$Comment = as.factor(data$Comment)

pt <- PivotTable$new()
pt$addData(data)
pt$addRowDataGroups("Species")
pt$addColumnDataGroups("Protocol")
pt$addColumnDataGroups("Comment")
pt$defineCalculation(calculationName="All", summariseExpression="n()", noDataCaption = "-")
pt$evaluatePivot()
pt$setStyling(rowNumbers = 1:(length(unique(data$Species)) + 1), 
              columnNumbers = 1:pt$columnCount,
              declarations=list("font-weight"="bold", "text-align"="center"))
pt$renderPivot()


