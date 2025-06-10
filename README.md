
<!-- README.md is generated from README.Rmd. Please edit that file -->

# projectmanagr

<!-- badges: start -->
<!-- badges: end -->

projectmanagr : A library for organising project documentation in R
Markdown.

This package facilitates the management of project documentation in R
Markdown files, including forming Project Documents, creating Project
Notes to meet Goals, Deliverables and Tasks in Project Documents, and
allowing the efficient storage and analysis of data within this
structure.

It comprises several modules:

## Filesystem Management

- **Filesystem Organisation & Management**

  - Organisation :

    - Standardise file & filesystem layout

  - Management :

    - Form and Maintain Cross-Referenced information
    - to SUMMARISE & ORGANISE

  - For comprehensive summary of functions See :

    - [FILESYSTEM MANAGEMENT](filesystem-management)

## Sample Management

- **Sample Management**

  - Plain-text Datatables & Databases

    - for data logging & processing/analysis in R

## Resource Management

- **Resource Management**

  - Inventory Rmd Files for managing resources

  - Protocol Rmd Files for managing processes

## Data Management

- **Data Management**

  - Volumes Rmd File to manage external data storage

  - Data Storage Log in Project Note Rmd

------------------------------------------------------------------------

# Installation

You can install the development version of projectmanagr from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("stevenjwest/projectmanagr")
```

------------------------------------------------------------------------

# Usage

The first step in using ProjectManagr is generating an Organisation -
comprising an organisation root directory & index file, and containing
one or more:

- Programmes : Collections of Projects with a defined scope

- Project Doc : Projects that consist of Goals, Deliverable, Tasks -
  linked to Project Notes

- Project Notes : Documentation of work to primarily achieve Project
  Goals/Deliverables/Tasks

------------------------------------------------------------------------

## Adding Google OAuth Client: Quick overview

Projectmanagr lets you pull Today’s Google-Calendar events straight into
your daily R Markdown journal, alongside extracted “TODO:” comments from
your project files, for daily planning & journaling.

Because Google Calendar is private data, each user must authorise
**their own** Google account.

The safest way is for every user to create a tiny “Desktop-app” OAuth
client in Google Cloud Console, then let projectmanagr read the Client’s
generated JSON file when it authenticates.

| **Why?** | **What you do** |
|----|----|
| *Give projectmanagr read-only access to **your** calendars*—nothing is shared with the package author; you stay in full control. | 1 Create (or reset) a **Desktop-app** OAuth client in Google Cloud Console. |
| *Keep secrets out of the repo*—the JSON lives only on your computer, not inside the package. | 2 Download the JSON; it contains a public `client_id` and a non-confidential `client_secret` that Google still requires for desktop apps. |
| *Let projectmanagr find the file automatically every session*—no hard-coding paths in scripts. | 3 Drop the file into your personal config folder shown by `rappdirs::user_config_dir("projectmanagr")` (e.g. `~/Library/Application Support/projectmanagr/`). |

Once setup, the first call to `extract_google_calendar_events()` opens a
browser for consent; a refresh-token is cached and future calls are
silent.

------------------------------------------------------------------------

### 1 Create / reset a Desktop-app OAuth client in Google Cloud Console

| step | what you click | why it matters |
|----|----|----|
| **1** | Open **Console → APIs & Services → Credentials** ([Configure a Google API Console Project for the Google Ads API](https://developers.google.com/google-ads/api/docs/oauth/cloud-project)) | The Credentials page lists every OAuth client you have. |
| **2** | **Choose your existing Desktop-app client** and click the **“Reset secret”** icon (two arrows) – **or** click **➕ Create credentials → OAuth client ID → Desktop app** to make a new one ([Configure a Google API Console Project for the Google Ads API](https://developers.google.com/google-ads/api/docs/oauth/cloud-project)) | Resetting generates a fresh `client_secret`; creating a new client gives you a clean ID/secret pair. |
| **3** | In the dialog that appears, click **Download JSON** (\[OAuth Desktop and Web Application Flows | Google Ads API\](<https://developers.google.com/google-ads/api/docs/client-libs/java/oauth-web>)) |
| **4** | *Optional:* under **OAuth consent screen** make sure **Publishing status = In production** so any Google account can authorise your app ([Configure the OAuth consent screen and choose scopes](https://developers.google.com/workspace/guides/configure-oauth-consent)) | Prevents “This app is in testing” warnings for your users. |

------------------------------------------------------------------------

### 2 Store the JSON in a per-user config folder

``` r
# to show the exact directory on this machine, execute this function:
rappdirs::user_config_dir("projectmanagr")
```

Typically the config directory is in these location on:

- macOS `~/Library/Application Support/projectmanagr/`  
- Windows `%APPDATA%\\projectmanagr\\`  
- Linux `~/.config/projectmanagr/`

1.  **Create that folder if it doesn’t exist.**  
2.  **Rename the download** to `gcal_oauth_client.json`.  
3.  **Move the file** into the folder.

`projectmanagr` looks for **exactly** that path; nothing is stored
inside the package, so secrets never live in version control.

Why this folder? `rappdirs::user_config_dir()` is the cross-platform
location recommended for end-user config files.

------------------------------------------------------------------------

### 3 How projectmanagr uses the file at run-time

``` r
cfg  <- file.path(rappdirs::user_config_dir("projectmanagr"),
                  "gcal_oauth_client.json")

googleAuthR::gar_set_client(json = cfg,
                            scopes = "https://www.googleapis.com/auth/calendar.readonly")

googleAuthR::gar_auth(email = TRUE)           # opens browser once
```

- `gar_set_client()` reads both `client_id` **and** `client_secret` and
  starts the standard Desktop-app flow that Google expects.

- Tokens are cached in `rappdirs::user_cache_dir("projectmanagr")`, so
  subsequent calls reuse the refresh token automatically.

------------------------------------------------------------------------

### 4 What each field in the JSON does

| key | purpose | safe to expose? |
|----|----|----|
| `client_id` | Identifies your app to Google’s auth server. | **Yes** – public. |
| `client_secret` | Confirms the app in the **token-exchange** step. Required for Desktop clients (\[Using OAuth 2.0 for Server to Server Applications | Authorization\](<https://developers.google.com/identity/protocols/oauth2/service-account>)). |
| `auth_uri` | Browser end-point where the user signs in. | Public. |
| `token_uri` | HTTPS end-point where R exchanges the auth code for tokens. | Public. |
| `redirect_uris` | Must include `http://localhost` so the local R session can receive the auth code. | Public. |

------------------------------------------------------------------------

### 5 Troubleshooting checklist

| symptom | fix |
|----|----|
| *“client_secret is missing”* | Ensure you used a **Desktop app** client and the JSON still contains `"client_secret": "GOCSPX-…"` |
| Browser opens twice | Delete the stale token cache: `unlink(rappdirs::user_cache_dir("projectmanagr"), recursive = TRUE)` and retry. |
| *invalid_scope* error | Make sure the Calendar API is **enabled** in the same Cloud project. |
| *“App isn’t verified”* banner | Publish the OAuth consent screen, or restrict scopes to read-only. |

------------------------------------------------------------------------

### 6 Security note — what the *client-secret* really means

Projectmanagr needs an OAuth **client-ID + client-secret** because
Google still requires both fields for Desktop-app credentials.

However, Google classifies Desktop secrets as *public*-information: they
can be embedded in every end-user machine and **cannot be kept
confidential** . Anyone who gets your JSON could call Google APIs *as
your application*, but **never as** **your Google account** — end-users
must still grant consent and tokens are short-lived.

**Why generate your own credential anyway?**

- Keeps your calendars under your own Google Cloud project and quota.  
- Prevents other Projectmanagr users from “impersonating” **your**
  application brand.  
- Lets you reset or revoke the secret at any time.

If you suspect the JSON leaked (committed, e-mailed, etc.) simply
**reset the** **client-secret**; previously issued refresh-tokens stop
working and a new JSON replaces the old one.

#### How to reset the client-secret (and download the new JSON)

1.  **Open Credentials page**  
    <https://console.cloud.google.com/apis/credentials>  
    This lists every OAuth client in the current project.

2.  **Select your Desktop-app client**  
    Click its name to open the details panel.

3.  **Reset secret**  
    In the top-right, click **“Reset Secret”** (two-arrow icon).  
    Google immediately generates a new secret and invalidates the old
    one.  
    *Console path: Credentials ▸ OAuth 2.0 Client IDs ▸
    **〈your-client〉** ▸ Reset Secret*

4.  **Download JSON**  
    After the reset, click **Download JSON**.  
    Save the file as  
    `rappdirs::user_config_dir("projectmanagr")/gcal_oauth_client.json`.  
    (On macOS that’s `~/Library/Application Support/projectmanagr/`.)

5.  **Restart R / rerun projectmanagr**  
    The next `extract_google_calendar_events()` call will pick up the
    new JSON, open a browser once for consent, and cache a fresh token.

> **Tip:** deleting the cached token directory  
> `unlink(rappdirs::user_cache_dir("projectmanagr"), recursive = TRUE)`  
> forces a brand-new OAuth dance with the new secret.

Now you—and only you—can authorise Projectmanagr to read your calendars,
and you can rotate or revoke that access whenever you like.

------------------------------------------------------------------------

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(projectmanagr)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

`devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
