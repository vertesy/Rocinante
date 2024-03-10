
"https://github.com/vertesy/Rocinante/issues/5"

"https://docs.posit.co/ide/server-pro/1.4.1102-1/server-management.html#suspending-sessions"

{
  source('~/GitHub/Packages/Rocinante/R/Rocinante.R'); (CBE.params <- getSLURMjobDetails())
  source("~/GitHub/Projects/CON/_sc6_21/Parameters.CON.sc6_21.R")
  source("~/GitHub/Projects/CON/_general/GeneLists.CON.R")
  source('~/ppp'); memory.biggest.objects(); OutDir
  create_set_Original_OutDir()
}
Maharaja <- 211

q(save = y, )
# Save workspace image to /groups/knoblich/users/abel.vertesy/cbehome/GitHub/Packages/Rocinante/.RData? [y/n/c]:

# Schedule a task to quit the R session at a specific time without needing a background job
# Install the later package if not already installed: install.packages("later")

library(later)

# Function to quit the R session
quit_session <- function() {
  (proj <- CodeAndRoll2::getProject())
  (date <- Stringendo::idate(Format = "%Y.%m.%d_%H.%M"))
  gc()
  session_name <- kpp("session", date, proj, "Rdata")
  save.image(file = session_name) # Optional: save session before quitting
  q("no") # Quit without saving (since we already saved manually)
}

# Schedule the session to quit at 09:20 AM on the current day
# Note: This assumes your R session and your system's time zone are the same
schedule_at <- function(target_hour, target_min, fun) {
  now <- Sys.time()
  target_time <- as.POSIXct(format(now, "%Y-%m-%d"), tz = attr(now, "tzone"))
  target_time <- target_time + hours(target_hour) + minutes(target_min)
  delay <- as.numeric(difftime(target_time, now, units = "secs"))

  if (delay < 0) {
    # Target time has passed for today, schedule for next day
    delay <- delay + 24 * 60 * 60
  }

  later(fun, delay)
}

# Example usage: Quit R session at 09:20 AM
schedule_at(9, 20, quit_session)
