


mod_time_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    rv_time <- reactiveValues()



    ##
    ## Introduction #########################################################
    ##

    ## Show/hide Introduction
    observeEvent(input$toggle_intro, {
      shinyjs::toggle(id = "time_intro", anim = T, animType = "slide")
    })



    ##
    ## checks from other modules ############################################
    ##

    output$test_approach <- renderText({

      if (is.null(rv$cv_model$cv_approach)) {
        "Approach selected: None."
      } else {
        paste0("Approach selected: ", rv$cv_model$cv_approach, ".")
      }

    })

    output$test_cv <- renderTable({ rv$cv_model$cv_mixed })



    ##
    ## Show/hide inputs #####################################################
    ##

    observe({

      req(rv$cv_model$cv_approach)

      if (rv$cv_model$cv_approach == "a2") {
        shinyjs::hide("check_approach")
        shinyjs::show("unit_times")
        shinyjs::show("box_time_to_params")
      } else {
        shinyjs::show("check_approach")
        shinyjs::hide("unit_times")
        shinyjs::hide("box_time_to_params")
      }

    })

    observe({

      req(rv$cv_model$cv_approach)
      req(rv$cv_model$cv_mixed)

      if (rv$cv_model$cv_approach == "a1") {
        shinyjs::hide("check_approach")
        shinyjs::show("unit_times")
        shinyjs::show("box_time_to_params")
      } else {
        shinyjs::show("check_approach")
        shinyjs::hide("unit_times")
        shinyjs::hide("box_time_to_params")
      }

    })



    ##
    ## Pass inputs to rv ####################################################
    ##

    observe({
      req(
        input$lvl1_dbh    , input$lvl2_dbh    , input$lvl3_dbh,
        input$lvl1_density, input$lvl2_density, input$lvl3_density,
        input$lvl1_time   , input$lvl2_time   , input$lvl3_time
        )

      rv$time$nested_plot <- tibble(
        nested_level = c("lvl1", "lvl2", "lvl3"),
        dbh_min      = c(input$lvl1_dbh    , input$lvl2_dbh    , input$lvl3_dbh    ),
        tree_density = c(input$lvl1_density, input$lvl2_density, input$lvl3_density),
        time_measure = c(input$lvl1_time   , input$lvl2_time   , input$lvl3_time   )
      )
    })

    observe({
      req(
        input$drive_time, input$walk_time, input$march_speed,
        input$auth_time, input$working_hour, input$working_day
      )

      rv$time$unit_times <- tibble(
        drive_time   = input$drive_time,
        walk_time    = input$walk_time,
        march_speed  = input$march_speed,
        auth_time    = input$auth_time,
        working_hour = input$working_hour,
        working_day  = input$working_day
      )
    })


    ##
    ## Prepare checks for default value #####################################
    ##

    observe({

      req(rv$time$nested_plot)

      rv_time$check_nested <- rv$time$nested_plot %>%
        mutate(
          check_dbh = case_when(
            nested_level == "lvl1" & dbh_min == 30 ~ 1,
            nested_level == "lvl2" & dbh_min == 10 ~ 1,
            nested_level == "lvl3" & dbh_min ==  2 ~ 1,
            TRUE ~ 0
          ),
          check_density = case_when(
            nested_level == "lvl1" & tree_density ==  300 ~ 1,
            nested_level == "lvl2" & tree_density == 1000 ~ 1,
            nested_level == "lvl3" & tree_density == 1500 ~ 1,
            TRUE ~ 0
          ),
          check_time = case_when(
            nested_level == "lvl1" & time_measure ==   3 ~ 1,
            nested_level == "lvl2" & time_measure ==   2 ~ 1,
            nested_level == "lvl3" & time_measure == 0.5 ~ 1,
            TRUE ~ 0
          )
        ) %>%
        dplyr::select(starts_with("check")) %>%
        mutate(across(.fns = ~replace(., . == 1, "Default value"))) %>%
        mutate(across(.fns = ~replace(., . == 0 , "User defined")))

      rv_time$check_nested_vec <- c(rv_time$check_nested$check_dbh,
                                    rv_time$check_nested$check_density,
                                    rv_time$check_nested$check_time)

      rv$time$check_nested_default <- if_else(
        identical(rv_time$check_nested_vec, rep("Default value", 9)),
        "All default values",
        "At least some user inputs"
      )

    })


    observe({

      req(rv$time$unit_times)

      rv_time$check_time <- rv$time$unit_times %>%
        mutate(
          drive_time   = if_else(drive_time   == 0.5, 1, 0),
          walk_time    = if_else(walk_time    ==   1, 1, 0),
          march_speed  = if_else(march_speed  ==   2, 1, 0),
          auth_time    = if_else(auth_time    ==   2, 1, 0),
          working_hour = if_else(working_hour ==   9, 1, 0),
          working_day  = if_else(working_day  ==  21, 1, 0)
        ) %>%
        mutate(across(.fns = ~replace(., . == 1, "Default value"))) %>%
        mutate(across(.fns = ~replace(., . == 0 , "User defined")))

      rv_time$check_time_vec <- as.character(rv_time$check_time)

      rv$time$check_time_default <- if_else(
        identical(rv_time$check_time_vec, rep("Default value", 6)),
        "All default values",
        "At least some user inputs"
      )

    })



    ##
    ## Make outputs #########################################################
    ##

    output$nested_check <- renderTable({
      req(rv_time$check_nested)
      rv_time$check_nested
      })

    output$other_check1 <- renderTable({
      req(rv_time$check_time)
      rv_time$check_time %>% dplyr::select(drive_time, walk_time, march_speed)
    })

    output$other_check2 <- renderTable({
      req(rv_time$check_time)
      rv_time$check_time %>% dplyr::select(auth_time, working_hour, working_day)
    })


    ##
    ## Change tab ###########################################################
    ##

    observeEvent(input$btn_to_params, {
      rv$to_params <- input$btn_to_params
    })




  }) ## END module server function

}
