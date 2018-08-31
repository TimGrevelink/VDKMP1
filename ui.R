dashboardPage(
  title = dashboard.naam,
  dashboardHeader(title = dashboard.naam
  ),
  dashboardSidebar(
    sidebarMenuOutput("ui_sidebar")
  ),
  dashboardBody(
    uiOutput("test_ui")
  )
)