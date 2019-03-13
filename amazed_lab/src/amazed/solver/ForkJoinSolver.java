package amazed.solver;

import amazed.maze.Maze;

import java.util.*;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * <code>ForkJoinSolver</code> implements a solver for
 * <code>Maze</code> objects using a fork/join multi-thread
 * depth-first search.
 * <p>
 * Instances of <code>ForkJoinSolver</code> should be run by a
 * <code>ForkJoinPool</code> object.
 */


public class ForkJoinSolver
        extends SequentialSolver {
    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal.
     *
     * @param maze the maze to be searched
     */
    public ForkJoinSolver(Maze maze) {
        super(maze);
    }

    public ForkJoinSolver(Maze maze, Map<Integer, Integer> predecessors, int start) {
        this.maze = maze;
        this.start = start;
        this.predecessor = predecessors;
        this.frontier = new Stack<>();
    }

    // All nodes that has been visited by any ForkJoinSolver
    static protected ConcurrentSkipListSet<Integer> visited = new ConcurrentSkipListSet<>();
    // A boolean to see if any ForkJoinSolver has found the goal
    static AtomicBoolean goalFound = new AtomicBoolean(false);
    // A list of all successors
    private List<ForkJoinSolver> successors = new ArrayList<>();


    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal, forking after a given number of visited
     * nodes.
     *
     * @param maze      the maze to be searched
     * @param forkAfter the number of steps (visited nodes) after
     *                  which a parallel task is forked; if
     *                  <code>forkAfter &lt;= 0</code> the solver never
     *                  forks new tasks
     */
    public ForkJoinSolver(Maze maze, int forkAfter) {
        this(maze);
        this.forkAfter = forkAfter;
    }

    /**
     * Searches for and returns the path, as a list of node
     * identifiers, that goes from the start node to a goal node in
     * the maze. If such a path cannot be found (because there are no
     * goals, or all goals are unreacheable), the method returns
     * <code>null</code>.
     *
     * @return the list of node identifiers from the start node to a
     * goal node in the maze; <code>null</code> if such a path cannot
     * be found.
     */
    @Override
    public List<Integer> compute() {
        return parallelSearch();
    }

    private List<Integer> parallelSearch() {
        // one player active on the maze at start
        int player = maze.newPlayer(start);
        // start with start node
        frontier.push(start);
        // as long as the player hasn't reached a dead end or the goal has been found
        while (!frontier.empty() && !goalFound.get()) {
            // get the new node to process
            int current = frontier.pop();
            // move player to current node, if it didn't already start on it (optimisation)
            if (current != start)
                maze.move(player, current);
            // if current node has a goal
            if (maze.hasGoal(current)) {
                // set the common variable to instigate that the goal has been found
                goalFound.set(true);
                // search finished: reconstruct and return subpath
                return pathFromTo(start,current);
            }
            // if current node has not been visited yet, mark node as visited
            if (ForkJoinSolver.visited.add(current)) {
                int visitedNbs = visitedNeighbours(maze.neighbors(current));
                // a variable to keep track of how many valid neighbours there are
                int nbs = (maze.neighbors(current)).size() - visitedNbs;
                // for every non-visited node nb adjacent to current
                for (int nb : maze.neighbors(current)) {
                    // if nb has not been already visited,
                    if (!ForkJoinSolver.visited.contains(nb)) {
                        // nb can be reached from current (i.e., current is nb's predecessor)
                        predecessor.put(nb, current);
                        // if there is a fork in the road, fork a new successor
                        if (nbs > 1) {
                            ForkJoinSolver f = new ForkJoinSolver(maze, predecessor, nb);
                            successors.add(f);
                            f.fork();
                        // if there wasn't a fork in the road, continue onwards
                        } else {
                            // add nb to the nodes to be processed
                            frontier.push(nb);
                        }
                    }
                }
                // if the player had successors, join them
                if (nbs > 1)
                    return joinChildren(current);
            }
        }

        // all nodes explored, no goal found
        return null;
    }

    /**
     * Joins all ForkJoinSolver's successors
     * @param current the current node the ForkJoinSolver is processing, used for path calculating
     * @return if a successor found a goal,
     * return the path from the successor added with the current ForkJoinSolver's traversed path
     * otherwise return null
     */
    private List<Integer> joinChildren(int current) {
        // for each successor
        for (ForkJoinSolver successor : successors) {
            // join them
            List<Integer> res = successor.join();
            // disown all successors that didn't find the goal
            if (res != null) {
                // create the ForkJoinSolver's path
                List<Integer> path = pathFromTo(start,current);
                // added with the path of the successor
                path.addAll(res);
                return path;

            }
        }
        // no successor found the goal
        return null;
    }

    /**
     * Calculate the amount of neighbours that has already been visited
     * @param nbs the neighbours to check if they've been visited
     * @return the amount of visited neighbours
     */
    private int visitedNeighbours(Set<Integer> nbs) {
        // initialize to 0
        int count = 0;
        // foreach neighbour
        for (int nb : nbs) {
            // if the neighbouring node has been visited
            if (ForkJoinSolver.visited.contains(nb))
                // add 1 to the counter
                count++;
        }
        return count;
    }
}
