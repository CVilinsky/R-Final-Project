
#Summed concern level agains the active cases
plot_active_cases_dates <- ggplot(filtered_summ,mapping = aes(x=Date,y=Active))+
  labs(title='Active Case',y="Active Cases")+
  xlab(NULL)+geom_line()+
  scale_y_continuous(trans=log2_trans(), breaks = trans_breaks("log2", function(x) 2^x),labels = trans_format("log2", math_format(2^.x)))
plot_polls_dates <- ggplot(civiqs_poll_data,aes(x=Date,y=sum_rep_dem))+geom_line()+labs(title='Concern Level',y='Level',x='Date')
grid.arrange(plot_active_cases_dates,plot_polls_dates,top=textGrob("The Effect of Active Cases on the Concern of the Citizens",gp=gpar(fontsize=15,font=2)))


#function to get the party of the state

get_party <- function(x){
  temp_vec <- grepl(x,df_governors$state_code)
  if (any(temp_vec)){
    row_num <- min(which(temp_vec==T))
    df_governors$party[row_num]
  }
  else{
    "None"
  }
} #given a state code, receive the party it's connected to

rep_list <-list()
list_index=1
rep_list[list_index] <- "Date"
list_index <- list_index+1
for (i in 2:ncol(df_wiki_filtered)){
  state <- colnames(df_wiki_filtered)[i]
  if(get_party(state)=="republican"){
    rep_list[list_index] <- state
    list_index <- list_index+1
  }
} #get list of all the republican states


rep_list <- unlist(rep_list)
republican_states <- df_wiki_filtered[rep_list] #dataframe with the republican states

longer_republican_states <- republican_states%>%pivot_longer(cols=-Date, names_to="State", values_to="Value")

ggplot(longer_republican_states, aes(x=Date,y= Value))+xlab(NULL)+
  geom_line(aes(color= "#DE0100"))+facet_wrap(.~State)+theme(legend.position="none")+
  labs(title="Covid-19 Infections In Republican States",subtitle = "Dates 21/01/2020 - 26/05/2021",y="Number Of Infections")

#infections per mounth

by_month <- summmarised_stats
by_month$y_m <- floor_date(by_month$Date,"month")

by_mounth <- by_month%>%group_by(y_m)%>%summarize(value=sum(Confirmed_daily))%>%as.data.frame() 

ggplot(by_mounth, aes(x=y_m,y=value))+geom_bar(stat='identity',fill="blue")+
  scale_x_date(date_breaks="1 month",labels= date_format("%y/%m"))+scale_y_continuous(labels=comma)+
  labs(title="Infections per Month",subtitle = "January 2020 - May 2021",y="Total Monthly Infections")+
  xlab(NULL)+ theme(plot.title = element_text(size=22), plot.subtitle = element_text(size=12))

#republican concern vs infections

rep_concern <- ggplot(civiqs_poll_data,aes(x=Date,y=rep))+geom_line(aes(color="#DE0100"))+
  labs(title = "Republican Concern Level",y="Concern Level")+
  xlab(NULL)+theme(legend.position="none")
  

republican_total_infections <- longer_republican_states%>%group_by(Date)%>%
  summarize(value=sum(Value))%>%as.data.frame()

rep_infections <- ggplot(republican_total_infections%>%filter(Date>="2020-02-25" & Date<="2020-04-05"),
                         aes(x=Date,y=value))+geom_line(aes(color="#DE0100"))+theme(legend.position="none")+
  labs(title="Total Infections in Republican States",y="Number Of Daily Infections")
grid.arrange(rep_concern,rep_infections,top=textGrob("The Effect of Daily Infections in Republican States", gp=gpar(fontsize=20,font=2)),
             bottom=textGrob("25/02/2020 - 05/04/2020", gp=gpar(fontsize=16)))
