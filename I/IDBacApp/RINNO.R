library("RInno")


get_R_exe(app_dir = "my/app/path", R_version = 2.2.1)

# Create batch file
create_bat(app_name = "My AppName", app_dir = "my/app/path")

# Create app config file
create_config(app_name = "My AppName", R_version = 2.2.1, app_dir = "my/app/path",
              pkgs = c("jsonlite", "shiny", "magrittr", "dplyr", "caret", "xkcd"))

# Build the iss script
start_iss(app_name = "My AppName") %>%

  # C-like directives
  directives(R_version   = 2.2.1,
             include_R   = TRUE,
             app_version = "0.1.2",
             publisher   = "Your Company",
             main_url    = "yourcompany.com") %>%

  # Setup Section
  setup(output_dir  = "wizard",
        app_version = "0.1.2",
        default_dir = "pf",
        privilege   = "high",
        inst_readme = "pre-install instructions.txt",
        setup_icon  = "myicon.ico",
        pub_url     = "mycompany.com",
        sup_url     = "mycompany.github.com/issues",
        upd_url     = "mycompany.github.com") %>%

  # Languages Section
  languages() %>%

  # Tasks Section
  tasks(desktop_icon = FALSE) %>%

  # Files Section
  files(app_dir = "my/app/path", file_list = "path/to/extra/files") %>%

  # Icons Section
  icons(app_desc       = "This is my local shiny app",
        app_icon       = "notdefault.ico",
        prog_menu_icon = FALSE,
        desktop_icon   = FALSE) %>%
