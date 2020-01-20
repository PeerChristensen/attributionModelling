# januar 2020

# channel_grouping function

custom_channel_grouping <- function(df) {
  
  library(tidylog)
  
  df <- df %>%
    mutate(channelGrouping = case_when(
      str_detect(source,"linkedin") & str_detect(medium,"cpc") ~ "Display (Linkedin) CPC",
      str_detect(medium, "^organic$") | str_detect(source,"^books\\.google\\.dk$") ~ "Organic Search",
      str_detect(medium,"cpc|CPC") & str_detect(campaign,"brand") & 
        str_detect(source, "bing|Bing") ~ "Direct (paid)",
      
      str_detect(medium,"display|^banner$") | str_detect(campaign,"display|Youtube") | 
        str_detect(source,"display|youtube\\.com|m\\.youtube\\.com") | 
        str_detect(sourceMedium, "youtube\\.com / referral") & 
        str_detect(campaign, "rmk",negate = T) ~ "Display (campaign)",
      
      str_detect(campaign,"Atcore - RMK - Abandon cart - (dynamisk)") | 
        str_detect(campaign,"Atcore - RMK - Abandon cart - (statisk)|Display - RMK - Abandoned cart - statisk") | 
        str_detect(campaign, "Display - RMK - Abandoned cart - dynamisk|Display - RMK - Abandoned product - dynamisk") |
        str_detect(sourceMedium,"remarketing-fb / facebook") | 
        str_detect(campaign,"Display - RMK - Abandoned visit - dynamisk") ~ "Display (remarketing)",
      
      str_detect(sourceMedium,"facebook / cpc|Rosinante / Facebook_paid") ~ "Display (Facebook)",
      
      str_detect(source,"facebook|Linkedin") |
        str_detect(medium, "facebook") |
        str_detect(sourceMedium, "instagram\\.com / referral|FB/IG / feed|IG / Story|Instagram / Story") ~ "Social",
      
      str_detect(source,"innometrics|newsletter|email / anbefalinger|email / (not set)|newsletter / banner") |
        str_detect(sourceMedium,"master list / (not set)|master list / anbefalinger") ~ "E-mail (automatic)",
      
      str_detect(source, "politiken plus|coop plus|matas|krimifan|peoplespress\\.dk|bookunibook") |
        str_detect(source,"Chris MacDonald|Danske Bioanalytikere|Agillic|coop|tidenskvinder\\.dk") |
        str_detect(source, "madbanditten|Unibogliste\\.dk|bageglad|umahro|Politikens Forlag") |
        str_detect(source, "TINKERBELL BOOKS ApS|People's Press|ProInvestor ApS") ~ "Partners",
      
      str_detect(medium,"^email$|^e-mail$") |
        str_detect(sourceMedium, "outlook\\.live.com / referral|Master List / email") |
        str_detect(source, "Master List|master list") &
        str_detect(source,"innometrics|^clubmatas$",negate = T) ~ "E-mail (manual)",
      
      str_detect(sourceMedium,"google / cpc|kelkoodk / cpc") &
        str_detect(campaign, "brand", negate = T) ~ "Adwords",
      
      str_detect(source,"bing|Bing") & str_detect(medium,"^cpc$") ~ "Bing (paid)",
      
      str_detect(source,"pensum\\.dk|Pensum\\.dk|bogpriser\\.dk|bog\\.nu|bogrobotten|bogpris.nu|Bogrobotten") ~"Prissammenligning",
      
      str_detect(medium, "^affiliate$") | str_detect(source, "tradedoubler|euroads|dba\\.dk|partner-ads|digitaladvisor") ~ "Affiliates",
      
      str_detect(medium, "^referral$") & str_detect(source,"saxo|facebook|books\\.google\\.dk|accounts\\.google\\.com",negate=T) ~ "Referral",
      
      TRUE ~ channelGrouping))
}
