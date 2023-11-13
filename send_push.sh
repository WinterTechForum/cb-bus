curl --request POST \
     --url https://onesignal.com/api/v1/notifications \
     --header "Authorization: Basic ${ONESIGNAL_API_KEY}" \
     --header 'accept: application/json' \
     --header 'content-type: application/json' \
     --data '
{
  "app_id" : "71326ce8-7c5b-4e52-a993-cd32c0605ba9",
  "included_segments": [
    "Total Subscriptions"
  ],
  "contents": {
    "en": "Generated from command line script",
    "es": "Generated from command line script in Spanish"
  },
  "name": "INTERNAL_CAMPAIGN_NAME"
}
'
