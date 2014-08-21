#source("./downloadData.R")
require(ggplot2)
## Graph 1: Cost of players by team
# Gives a view of player cost variation 
# and which teams have highest rated players (when cost is used as proxy)

ggplot(ap, aes(x = now_cost, y = status)) + 
    geom_point()

## Graph 1.b: Cost of players by type

ggplot(ap, aes(x = type_now, y = now_cost)) + 
    geom_point()

## Graph 2: Average values for players from a team
# Provides a view of most expensive teams 
# (Cost also provides a proxy view of team quality)

ggplot(ap,aes(x=team_name,y=now_cost)) + 
    stat_summary(fun.y="mean", geom="point")

## Graph 3: Count of players in a team
ggplot(ap,aes(x=factor(team_name))) + geom_bar()

## 3.b Frequency
ggplot(ap,aes(x=factor(team_name))) + 
    geom_bar(aes(y=(..count../sum(..count..))))


# 4.a: Teams arranged by cost of players

ggplot(team_details,aes(x=reorder(team_name,totcost),y=totcost)) + 
    geom_bar(stat="identity",color="black") + coord_flip()

ggplot(team_details,aes(x=reorder(team_name,totcost),y=totcost,fill=totcost)) + 
    geom_bar(stat="identity",color="black") + coord_flip()

ggplot(team_details,aes(x=reorder(team_name,avgcost),y=avgcost,fill=avgcost)) + 
    geom_bar(stat="identity",color="black") + coord_flip()

ggplot(team_details,aes(x=reorder(team_name,maxcost),y=maxcost,fill=maxcost)) + 
    geom_bar(stat="identity",color="black") + coord_flip()

ggplot(team_details,aes(x=reorder(team_name,mincost),y=mincost,fill=mincost)) + 
    geom_bar(stat="identity",color="black") + coord_flip()


