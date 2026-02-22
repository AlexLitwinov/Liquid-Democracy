library(igraph)
library(tibble)
library(dplyr)

# -----------------------------
# Helpers: attractiveness + weighted choice
# -----------------------------
compute_attractiveness <- function(pref_i, pref_j, count_i, count_j, resp) {
  pref_sim <- 1 - abs(pref_i - pref_j)
  power_diff <- count_j - count_i
  pref_sim / (1 + exp(-resp * power_diff))
}

weighted_choice <- function(ids, weights) {
  # safety: avoid NA / negative / all-zero weights
  w <- pmax(weights, 0)
  if (all(!is.finite(w)) || sum(w, na.rm = TRUE) <= 0) {
    return(sample(ids, 1))
  }
  sample(ids, size = 1, prob = w)
}

# -----------------------------
# NetLogo: setup-nodes + setup-experts
# -----------------------------
setup_agents <- function(number_of_nodes_per_community,
                         number_of_communities,
                         number_of_experts_per_community,
                         seed = 1) {
  set.seed(seed)
  
  number_of_nodes <- number_of_nodes_per_community * number_of_communities
  number_of_experts <- number_of_experts_per_community * number_of_communities
  number_of_nodes_ALL <- number_of_nodes + number_of_experts
  
  agents <- tibble(
    id = 1:number_of_nodes_ALL,
    type = c(rep("lay", number_of_nodes), rep("expert", number_of_experts)),
    preference = runif(number_of_nodes_ALL),
    delegated = FALSE,
    my_vote = NA_real_,
    countdelegations = 0L
  )
  
  # Communities exactly like NetLogo:
  # laypersons: community = who mod number_of_communities
  agents$community <- NA_integer_
  lay_ids <- which(agents$type == "lay")
  agents$community[lay_ids] <- (lay_ids - 1) %% number_of_communities
  
  # experts: expert-who = who - number_of_nodes; community = expert-who mod number_of_communities
  exp_ids <- which(agents$type == "expert")
  expert_who <- (exp_ids - 1) - number_of_nodes  # 0-based
  agents$community[exp_ids] <- expert_who %% number_of_communities
  
  list(
    agents = agents,
    number_of_nodes = number_of_nodes,
    number_of_experts = number_of_experts,
    number_of_nodes_ALL = number_of_nodes_ALL
  )
}

# -----------------------------
# NetLogo: setup-network (circle network per community) + optional MS-rewiring
# -----------------------------
setup_friendship_ring <- function(agents,
                                  number_of_nodes,              # lay only
                                  number_of_communities,
                                  node_degree,
                                  MS_rewiring_iterations = 0,
                                  seed = 1) {
  stopifnot(node_degree %% 2 == 0)
  
  set.seed(seed)
  
  lay_ids <- which(agents$type == "lay")
  
  edges <- vector("list", length = 0)
  
  # For each community, build a ring-lattice:
  # connect each node to +/- 1..(node_degree/2) steps within that community ring.
  for (c in 0:(number_of_communities - 1)) {
    nodes_c <- lay_ids[agents$community[lay_ids] == c]
    n_c <- length(nodes_c)
    if (n_c == 0) next
    
    k <- node_degree / 2
    
    # order is by id (NetLogo effectively orders by who)
    nodes_c <- sort(nodes_c)
    
    for (idx in seq_len(n_c)) {
      from <- nodes_c[idx]
      for (s in 1:k) {
        to <- nodes_c[((idx - 1 + s) %% n_c) + 1]  # forward
        # NetLogo creates both to and from links, i.e. directed both ways
        edges[[length(edges) + 1]] <- c(from, to)
        edges[[length(edges) + 1]] <- c(to, from)
      }
    }
  }
  
  edges_df <- as_tibble(do.call(rbind, edges), .name_repair = "minimal") |>
    setNames(c("from", "to")) |>
    distinct()
  
  gF <- graph_from_data_frame(edges_df, directed = TRUE, vertices = agents)
  
  # edge attributes like NetLogo
  E(gF)$attractiveness <- 0
  E(gF)$delegation <- FALSE
  
  # --- MS rewiring (approximation of your NetLogo MS-rewiring) ---
  # We implement: pick A->B and C->D (neighbors), then rewire A->D and B->C if edges absent.
  if (MS_rewiring_iterations > 0) {
    for (it in seq_len(MS_rewiring_iterations)) {
      A <- sample(lay_ids, 1)
      neighA <- neighbors(gF, A, mode = "out") |> as.integer()
      if (length(neighA) == 0) next
      B <- sample(neighA, 1)
      
      C <- sample(lay_ids, 1)
      neighC <- neighbors(gF, C, mode = "out") |> as.integer()
      if (length(neighC) == 0) next
      D <- sample(neighC, 1)
      
      # constraints (mirror NetLogo conditions)
      if (length(unique(c(A, B, C, D))) < 4) next
      if (are.connected(gF, A, D) || are.connected(gF, B, C)) next
      
      # delete A<->B and C<->D (both directions), add A<->D and B<->C (both directions)
      gF <- delete_edges(gF, E(gF)[.from(A) & .to(B)])
      gF <- delete_edges(gF, E(gF)[.from(B) & .to(A)])
      gF <- delete_edges(gF, E(gF)[.from(C) & .to(D)])
      gF <- delete_edges(gF, E(gF)[.from(D) & .to(C)])
      
      gF <- add_edges(gF, c(A, D, D, A, B, C, C, B))
    }
  }
  
  gF
}

# -----------------------------
# NetLogo: setup-experts (experts create friendship-to themselves from laypersons in same community)
# -----------------------------
connect_experts <- function(gF, agents, number_of_nodes_per_community, expert_connectedness, seed = 1) {
  set.seed(seed)
  
  lay_ids <- which(agents$type == "lay")
  exp_ids <- which(agents$type == "expert")
  
  for (e in exp_ids) {
    c <- agents$community[e]
    lay_in_c <- lay_ids[agents$community[lay_ids] == c]
    connections_needed <- ceiling(expert_connectedness * number_of_nodes_per_community)
    connections_needed <- min(connections_needed, length(lay_in_c))
    if (connections_needed <= 0) next
    
    chosen <- sample(lay_in_c, connections_needed)
    # create friendship-to myself: lay -> expert (directed)
    gF <- add_edges(gF, as.vector(rbind(chosen, rep(e, length(chosen)))))
  }
  
  # initialize edge attrs for new edges
  E(gF)$attractiveness[is.na(E(gF)$attractiveness)] <- 0
  E(gF)$delegation[is.na(E(gF)$delegation)] <- FALSE
  
  gF
}

# -----------------------------
# NetLogo: count-delegations (reverse radius == in-component size)
# -----------------------------
count_delegations <- function(agents, gD) {
  n <- nrow(agents)
  counts <- integer(n)
  
  for (v in 1:n) {
    counts[v] <- length(subcomponent(gD, v, mode = "in"))
  }
  agents$countdelegations <- counts
  agents
}

# -----------------------------
# NetLogo: get-my-vote (propagate root preference backward; cycles remain NA)
# -----------------------------
get_my_vote <- function(agents, gD) {
  n <- nrow(agents)
  votes <- rep(NA_real_, n)
  
  roots <- which(degree(gD, mode = "out") == 0)
  
  for (r in roots) {
    reachable <- subcomponent(gD, r, mode = "in")
    votes[reachable] <- agents$preference[r]
  }
  
  agents$my_vote <- votes
  agents
}


# -----------------------------
# NetLogo: update-delegation (single layperson)
# -----------------------------
update_delegation <- function(i, agents, gF, gD, responsiveness_to_power) {
  
  # reset outgoing delegation from i
  gD <- delete_edges(gD, E(gD)[.from(i)])
  agents$delegated[i] <- FALSE
  
  # update attractiveness of i's outgoing friendships (on-the-fly)
  neigh <- neighbors(gF, i, mode = "out") |> as.integer()
  
  # option 1: self (no delegation)
  options_to <- c(i)
  options_w  <- c(1 / (1 + exp(-responsiveness_to_power * agents$countdelegations[i])))
  
  # options: friends
  if (length(neigh) > 0) {
    for (j in neigh) {
      w <- compute_attractiveness(
        pref_i = agents$preference[i],
        pref_j = agents$preference[j],
        count_i = agents$countdelegations[i],
        count_j = agents$countdelegations[j],
        resp = responsiveness_to_power
      )
      options_to <- c(options_to, j)
      options_w  <- c(options_w, w)
      
      # store attractiveness on the friendship edge i->j (like NetLogo)
      eid <- get.edge.ids(gF, c(i, j), directed = TRUE, error = FALSE)
      if (eid != 0) E(gF)$attractiveness[eid] <- w
    }
  }
  
  chosen <- weighted_choice(options_to, options_w)
  
  if (chosen != i) {
    gD <- add_edges(gD, c(i, chosen))
    agents$delegated[i] <- TRUE
    
    # mark the friendship edge as "delegation? = true" (cosmetic / bookkeeping)
    eid <- get.edge.ids(gF, c(i, chosen), directed = TRUE, error = FALSE)
    if (eid != 0) E(gF)$delegation[eid] <- TRUE
  }
  
  list(agents = agents, gF = gF, gD = gD, chosen = chosen)
}

lostvotes <- function(agents) {
  mean(is.na(agents$my_vote))
}

# -----------------------------
# NetLogo: setup + go loop
# -----------------------------
simulate_liquid_democracy <- function(
    seed = 123,
    number_of_nodes_per_community = 50,
    number_of_communities = 5,
    node_degree = 6,
    number_of_experts_per_community = 2,
    expert_connectedness = 0.2,
    MS_rewiring_iterations = 0,
    responsiveness_to_power = 1,
    T = 200
) {
  
  # setup
  st <- setup_agents(
    number_of_nodes_per_community = number_of_nodes_per_community,
    number_of_communities = number_of_communities,
    number_of_experts_per_community = number_of_experts_per_community,
    seed = seed
  )
  agents <- st$agents
  number_of_nodes <- st$number_of_nodes
  number_of_nodes_ALL <- st$number_of_nodes_ALL
  
  gF <- setup_friendship_ring(
    agents = agents,
    number_of_nodes = number_of_nodes,
    number_of_communities = number_of_communities,
    node_degree = node_degree,
    MS_rewiring_iterations = MS_rewiring_iterations,
    seed = seed
  )
  
  gF <- connect_experts(
    gF = gF,
    agents = agents,
    number_of_nodes_per_community = number_of_nodes_per_community,
    expert_connectedness = expert_connectedness,
    seed = seed
  )
  
  gD <- make_empty_graph(n = number_of_nodes_ALL, directed = TRUE)
  gD <- set_vertex_attr(gD, "name", value = agents$id)
  
  # initial counts/votes
  agents <- count_delegations(agents, gD)
  agents <- get_my_vote(agents, gD)
  
  lay_ids <- which(agents$type == "lay")
  history_lost <- numeric(T)
  
  # go
  for (t in seq_len(T)) {
    i <- sample(lay_ids, 1)
    
    upd <- update_delegation(
      i = i,
      agents = agents,
      gF = gF,
      gD = gD,
      responsiveness_to_power = responsiveness_to_power
    )
    agents <- upd$agents
    gF <- upd$gF
    gD <- upd$gD
    
    agents <- count_delegations(agents, gD)
    agents <- get_my_vote(agents, gD)
    
    history_lost[t] <- lostvotes(agents)
  }
  
  list(
    agents = agents,
    friendship_graph = gF,
    delegation_graph = gD,
    lostvotes = history_lost
  )
}
