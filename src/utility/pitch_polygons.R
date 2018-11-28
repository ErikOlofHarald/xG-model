pitch_polygons <- function(xmax = 10500, ymax = 6800, xmin = 0, ymin = 0) {
  
  options(stringsAsFactors = FALSE)
  
  # Standard dimensions
  goal_width <- 732
  penalty_spot <- 1100
  box_18y_w <- 4032
  box_18y_h <- 1650
  box_6y_w <- 1832
  box_6y_h <- 550
  circle_r <- 915
  corner_r <- 100
  pitch_w <- (xmax - xmin)
  pitch_h <- (ymax - ymin)
  
  # Functions
  arcs_fun <- function(c, r, start, stop, n, id, add_center = FALSE) {
    rad <- seq(start, stop, length.out = n)
    x <- c[1] + r * cos(rad)
    y <- c[2] + r * sin(rad)
    if (add_center)
      data.frame(id = rep(id, n + 1), x = c(x, c[1]), y = c(y, c[2]))
    else
      data.frame(id = rep(id, n), x = x, y = y)
  }
  
  box_fun <- function(x, y, xend, yend, id) {
    data.frame(
      x = c(x, x, xend, xend),
      y = c(y, yend, yend, y),
      id = rep(id, 4)
    )
  }
  
  # Boxes
  boxes <- rbind(
    box_fun(
      x = xmin,
      y = pitch_h / 2 - box_18y_w / 2,
      xend = box_18y_h,
      yend = pitch_h / 2 + box_18y_w / 2,
      id = "18 yard box left"
    ),
    box_fun(
      x = xmax - box_18y_h,
      y = pitch_h / 2 - box_18y_w / 2,
      xend = xmax,
      yend = pitch_h / 2 + box_18y_w / 2,
      id = "18 yard box right"
    ),
    box_fun(
      x = xmin,
      y = pitch_h / 2 - box_6y_w / 2,
      xend = box_6y_h,
      yend = pitch_h / 2 + box_6y_w / 2,
      id = "6 yard box left"
    ),
    box_fun(
      x = xmax - box_6y_h,
      y = pitch_h / 2 - box_6y_w / 2,
      xend = xmax,
      yend = pitch_h / 2 + box_6y_w / 2,
      id = "6 yard box right"
    )
  )
  
  # Goals
  goals <- rbind(
    box_fun(
      x = xmin - 200,
      y = pitch_h / 2 - goal_width / 2,
      xend = xmin,
      yend = pitch_h / 2 + goal_width / 2,
      id = "goal left"
    ),
    box_fun(
      x = xmax,
      y = pitch_h / 2 - goal_width / 2,
      xend = xmax + 200,
      yend = pitch_h / 2 + goal_width / 2,
      id = "goal right"
    )
  )
  
  # Pitch
  pitch <- rbind(
    box_fun(
      x = xmin,
      y = ymin,
      xend = pitch_w / 2,
      yend = ymax,
      id = "pitch left"
    ),
    box_fun(
      x = pitch_w / 2,
      y = ymin,
      xend = xmax,
      yend = ymax,
      id = "pitch right"
    )
  )
  
  # Arcs
  arc_angle <- asin((box_18y_h - penalty_spot) / circle_r)
  
  arcs <- rbind(
    arcs_fun(
      c = c(penalty_spot, pitch_h / 2),
      r = circle_r,
      start = -pi / 2 + arc_angle,
      stop = pi / 2 - arc_angle,
      n = 500,
      id = "left arc"
    ),
    arcs_fun(
      c = c(xmax - penalty_spot, pitch_h / 2),
      r = circle_r,
      start = pi / 2 + arc_angle,
      stop =  3 * pi / 2 - arc_angle,
      n = 500,
      id = "right arc"
    ),
    arcs_fun(
      c = c(xmin, ymin),
      r = corner_r,
      start = 0,
      stop =  pi / 2,
      n = 100,
      id = "lower left corner",
      add_center = TRUE
    ),
    arcs_fun(
      c = c(xmin, ymax),
      r = corner_r,
      start = -pi / 2,
      stop = 0,
      n = 100,
      id = "upper left corner",
      add_center = TRUE
    ),
    arcs_fun(
      c = c(xmax, ymax),
      r = corner_r,
      start = pi,
      stop = 3 * pi / 2,
      n = 100,
      id = "upper right corner",
      add_center = TRUE
    ),
    arcs_fun(
      c = c(xmax, ymin),
      r = corner_r,
      start = pi / 2,
      stop = pi,
      n = 100,
      id = "lower right corner",
      add_center = TRUE
    )
  )
  
  # Circles
  circles <- rbind(
    arcs_fun(
      c = c(pitch_w / 2, pitch_h / 2),
      r = circle_r,
      start = 0,
      stop = 2 * pi,
      n = 1000,
      id = "center circle"
    )
  )
  
  # Points
  points <- rbind(
    arcs_fun(
      c = c(penalty_spot, pitch_h / 2),
      r = 20,
      start = 0,
      stop = 2 * pi,
      n = 100,
      id = "left penalty spot"
    ),
    arcs_fun(
      c = c(xmax - penalty_spot, pitch_h / 2),
      r = 20,
      start = 0,
      stop = 2 * pi,
      n = 100,
      id = "right penalty spot"
    ),
    arcs_fun(
      c = c(pitch_w / 2, pitch_h / 2),
      r = 20,
      start = 0,
      stop = 2 * pi,
      n = 100,
      id = "center spot"
    )
  )
  
  options(stringsAsFactors = TRUE)

  rbind(boxes, goals, pitch, arcs, circles, points)
}