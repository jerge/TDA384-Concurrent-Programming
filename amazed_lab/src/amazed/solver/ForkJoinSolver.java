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

    static protected ConcurrentSkipListSet<Integer> visited = new ConcurrentSkipListSet<>();
    static AtomicBoolean goalFound = new AtomicBoolean(false);
    private List<ForkJoinSolver> children = new ArrayList<>();


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

        // as long as not all nodes have been processed
        while (!frontier.empty() && !goalFound.get()) {
            // get the new node to process
            int current = frontier.pop();
            // move player to current node
            if (current != start) {
                maze.move(player, current);
            }
            // if current node has a goal
            if (maze.hasGoal(current)) {
                System.out.println("I won");
                goalFound.set(true);
                // search finished: reconstruct and return path
                return pathFromTo(start,current);
            }
            // if current node has not been visited yet
            if (!ForkJoinSolver.visited.contains(current)) {
                // mark node as visited
                ForkJoinSolver.visited.add(current);
                // for every node nb adjacent to current
                int fakenbs = visitedNeighbours(maze.neighbors(current));
                int nbs = (maze.neighbors(current)).size() - fakenbs;
                System.out.println(nbs);
                for (int nb : maze.neighbors(current)) {

                    // add nb to the nodes to be processed

                    // if nb has not been already visited,
                    // nb can be reached from current (i.e., current is nb's predecessor)
                    if (!ForkJoinSolver.visited.contains(nb)) {
                        predecessor.put(nb, current);
                        if (nbs > 1) {
                            System.out.println("forked");
                            ForkJoinSolver f = new ForkJoinSolver(maze, predecessor, nb);
                            children.add(f);
                            f.fork();
                        }
                        else if (nbs == 0) {
                            //join();
                        } else {
                            frontier.push(nb);
                        }
                    }
                }
                if (nbs > 1) {
                    return joinChildren(current);
                }
            }
        }

        // all nodes explored, no goal found
        return null;
    }

    private List<Integer> joinChildren(int current) {
        for (ForkJoinSolver child : children) {
            List<Integer> res = child.join();
            if (res != null) {
                List<Integer> path = pathFromTo(start,current);
                path.addAll(res);
                return path;
                
            }
        }
        return null;
    }

    private int visitedNeighbours(Set<Integer> nbs) {
        int count = 0;
        for (int nb : nbs) {
            if (ForkJoinSolver.visited.contains(nb)) {
                count++;
            }
        }
        //System.out.println(count);
        return count;
    }
}
