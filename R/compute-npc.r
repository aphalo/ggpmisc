compute_npc <- function(x, margin.npc = 0.05) {
  stopifnot(is.character(x))
  npc.positions <- c(right = 1 - margin.npc,
                     left = 0 + margin.npc,
                     centre = 0.5,
                     center = 0.5,
                     middle = 0.5,
                     top = 1 - margin.npc,
                     bottom = 0 + margin.npc)
  unname(npc.positions[x])
}
