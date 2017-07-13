
from datetime import datetime as dt
import imp
import os
import numpy as np
import pandas as pd
from slacker import Slacker
import config

slack = Slacker(os.environ.get("SLACK_API_TOKEN"))

db = imp.load_source('db', os.path.realpath(os.path.join(".", '../people/eric/db.py')))
db_read_only = db.Db(db_url=config.db_url["read_only"])

def get_recent_logs(current_time, minutes_since_creation):
  """Retrieve most recent staffing level update logs"""
  qry = """
  select created_at, params->'zone_id' as zone_id, params, summaries
  from logistics_staffing_logs
  where '%s' - created_at < '%s minutes'
  limit 100
  """ % (current_time, minutes_since_creation)
  df = db_read_only.to_dataframe(qry)
  return df

sl_logs = get_recent_logs(dt.now(), config.minutes)

for z in config.zone_ids:
  rows = sl_logs.loc[sl_logs['zone_id'] == z].shape[0]
  try:
    if rows == 0:
      raise Exception("Missing recent staffing levels update for zone " + str(z))
  except Exception as e:
    if __name__ == "__main__":
      config.rollbar.report_exc_info()
      if config.slack_enabled:
        slack.chat.post_message("#schedulinglogistics",
          "<!channel> Just FYI, the staffing levels update did not complete this morning. " +
          "Engineering has been alerted.",
          username="staffing-levels-bot",
          icon_emoji=":dart:")
    raise e

print "Done!"
