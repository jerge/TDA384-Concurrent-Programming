import TSim.*;
import javafx.geometry.Point2D;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Semaphore;

import static java.lang.Math.min;

public class Lab1 {

  private Train train1;
  private Train train2;

  private int simulatorSpeed;

  private Semaphore cross = new Semaphore(1);
  private Semaphore up = new Semaphore(1);
  private Semaphore left = new Semaphore(1);
  private Semaphore mid = new Semaphore(1);
  private Semaphore right = new Semaphore(1);
  private Semaphore down = new Semaphore(1);

  private enum SensorName {
    // Station
    /*NORTHNORTHSTATION,
    NORTHSOUTHSTATION,
    SOUTHNORTHSTATION,
    SOUTHSOUTHSTATION,*/
    // Crossing
    NORTHCROSS,
    WESTCROSS,
    EASTCROSS,
    SOUTHCROSS,
    // Up Semaphore
    WESTUP,
    //EASTUP,
    SOUTHUP,
    // Left Semaphore
    WESTLEFT,
    EASTLEFT,
    SOUTHLEFT,
    // Right Semaphore
    WESTRIGHT,
    EASTRIGHT,
    SOUTHRIGHT,
    // Down Semaphore
    EASTDOWN,
    SOUTHDOWN
  }

  // Position to sensor map
  private Map<Point2D,SensorName> posSensor = new HashMap<>();

  public Lab1(int simulatorSpeed, int speed1, int speed2) {
    this.simulatorSpeed = simulatorSpeed;
    TSimInterface tsi = TSimInterface.getInstance();
    train1 = new Train(1,tsi,speed1);
    train2 = new Train(2,tsi,speed2);
    System.out.println(speed1 + ", " + speed2);
    try {
      train1.setSpeed(speed1);
      train2.setSpeed(speed2);

      init();
    }
    catch (CommandException e) {
      e.printStackTrace();    // or only e.getMessage() for the error
      System.exit(1);
    }

    train1.start();
    train2.start();

  }

  /**
   * Initializes the HashMap that couples the SensorName to a location
   */
  private void init(){
    /*posSensor.put(new Point2D(15,3), SensorName.NORTHNORTHSTATION);
    posSensor.put(new Point2D(15,5), SensorName.NORTHSOUTHSTATION);

    posSensor.put(new Point2D(15,11), SensorName.SOUTHNORTHSTATION);
    posSensor.put(new Point2D(15,13), SensorName.SOUTHSOUTHSTATION);*/

    posSensor.put(new Point2D(9,5), SensorName.NORTHCROSS);
    posSensor.put(new Point2D(6,6), SensorName.WESTCROSS);
    posSensor.put(new Point2D(11,7), SensorName.EASTCROSS);
    posSensor.put(new Point2D(10,8), SensorName.SOUTHCROSS);

    posSensor.put(new Point2D(14,7), SensorName.WESTUP);
    posSensor.put(new Point2D(15,8), SensorName.SOUTHUP);

    posSensor.put(new Point2D(13,9), SensorName.WESTRIGHT);
    posSensor.put(new Point2D(19,9), SensorName.EASTRIGHT);
    posSensor.put(new Point2D(13,10), SensorName.SOUTHRIGHT);

    posSensor.put(new Point2D(1,9), SensorName.WESTLEFT);
    posSensor.put(new Point2D(7,9), SensorName.EASTLEFT);
    posSensor.put(new Point2D(6,10), SensorName.SOUTHLEFT);

    posSensor.put(new Point2D(6,11), SensorName.EASTDOWN);
    posSensor.put(new Point2D(4,13), SensorName.SOUTHDOWN);

  }

  private class Train extends Thread{
    private final int id;
    private final TSimInterface tsim;
    // 1 is starting direction, -1 is backwards direction
    private int direction = 1;
    private int speed = 0;
    private final int maxSpeed;

    // Saves the last activated sensor to know which direction you came from
    private SensorName lastTrippedSensor = null;

    // Saves semaphores to make sure you only release if you own the semaphore
    private List<Semaphore> semaphores = new ArrayList<>(2);

    Train(int id, TSimInterface tsim, int maxSpeed) {
      this.id = id;
      this.tsim = tsim;
      this.maxSpeed = maxSpeed;
    }

    /**
     * Sets the speed of the train to the speed * direction
     * @param spd the speed to set the train to
     * @throws CommandException
     */
    void setSpeed(int spd) throws CommandException{
      spd = min(maxSpeed, spd);
      this.speed = spd;
      tsim.setSpeed(id, speed*direction);
    }

    /**
     * Stops the train in 200 (arbitrary constant)*railTracks/speed*simulatorSpeed milliseconds,
     * waits for 1000+(20*speed) amount of milliseconds. Then changes the direction
     * and sets the speed to the maxSpeed
     * @param railTracks the amount of railTracks until the train shall turn
     * @throws InterruptedException
     * @throws CommandException
     */
    void stopAtStation(int railTracks) throws InterruptedException, CommandException{
      sleep(200*railTracks/speed*simulatorSpeed);
      setSpeed(0);
      sleep(1000 + (20 * speed)*simulatorSpeed);
      direction *= (-1);
      setSpeed(maxSpeed);
    }

    /**
     * Tries to acquire the semaphore and stops and waits until it can acquire it if it failed
     * @param semaphore the Semaphore to be acquired
     * @throws InterruptedException
     * @throws CommandException
     */
    void waitForAcquire(Semaphore semaphore) throws InterruptedException, CommandException {
      if (!semaphore.tryAcquire()){
        setSpeed(0);
        semaphore.acquire();
        setSpeed(maxSpeed);
        semaphores.add(semaphore);
      }
    }

    /**
     * Releases the specified semaphore and removes it from the train's @sempahores list
     * @param semaphore the Semaphore to be released
     */
    void releaseSemaphore(Semaphore semaphore) {
      semaphore.release();
      semaphores.remove(semaphore);
    }

    /**
     * Tries to acquire the Semaphore and sets the switch at the location to the specified direction
     * or if it fails to acquire, it sets the switch to the opposite of the specified direction
     * @param semaphore the Semaphore to be tried to acquire
     * @param x the x position of the switch
     * @param y the y position of the switch
     * @param dir the direction the train should set the switch if it acquires the semaphore
     * @throws CommandException
     */
    void tryToAcquire(Semaphore semaphore, int x, int y, int dir) throws CommandException{
      if (semaphore.tryAcquire()) {
        semaphores.add(semaphore);
        tsim.setSwitch(x,y,dir);
      } else {
        dir = dir%2+1;  // Invert direction
        tsim.setSwitch(x,y,dir);
      }
    }

    /**
     * Does different actions for each sensor when one is activated (does nothing upon inactive).
     * Upon reaching a semaphore either the train waits to acquire it and then continues after
     * achieving it.
     * Or the train decides direction whether the semaphore is available.
     * After passing it makes sure it releases the semaphore.
     * @param sensor the SensorEvent which has happened
     * @throws InterruptedException
     * @throws CommandException
     */
    synchronized void pollSensorEvent(SensorEvent sensor) throws InterruptedException, CommandException{
      SensorName sensorName = posSensor.get((new Point2D(sensor.getXpos(),sensor.getYpos())));
      if (sensor.getStatus() == SensorEvent.ACTIVE) { // Only do action when you activate a sensor
        switch (sensorName) {                         // Depending on the sensor, do different stuff
          // At Semaphores make sure you: Acquire the semaphore to pass, or release it if you have passed
          // At stations stop in a few seconds (depending on the steps and speed) and turn around.
          // In some cases you have to wait in order to acquire,
          //  other times you have to switch direction
          /*case NORTHNORTHSTATION:
          case NORTHSOUTHSTATION:
          case SOUTHNORTHSTATION:
          case SOUTHSOUTHSTATION:
            break;*/
          case NORTHCROSS:
            if (lastTrippedSensor == SensorName.SOUTHCROSS){
              releaseSemaphore(cross);
              stopAtStation(6);
            }
            else waitForAcquire(cross);
            break;
          case WESTCROSS:
            if (lastTrippedSensor == SensorName.EASTCROSS) {
              releaseSemaphore(cross);
              stopAtStation(12);
            }
            else waitForAcquire(cross);
            break;
          case EASTCROSS:
            if (lastTrippedSensor == SensorName.WESTUP) waitForAcquire(cross);
            else releaseSemaphore(cross);
            break;
          case SOUTHCROSS:
            if (lastTrippedSensor == SensorName.SOUTHUP) waitForAcquire(cross);
            else releaseSemaphore(cross);
            break;
          case WESTUP:
            if (lastTrippedSensor == SensorName.EASTCROSS) {
              waitForAcquire(right);
              tsim.setSwitch(17,7,2);
            } else releaseSemaphore(right);
            break;
          case SOUTHUP:
            if ( lastTrippedSensor == SensorName.SOUTHCROSS) {
              waitForAcquire(right);
              tsim.setSwitch(17,7,1);
            } else releaseSemaphore(right);
            break;
          case WESTLEFT:
            if (lastTrippedSensor == SensorName.SOUTHDOWN) releaseSemaphore(down);
            else if (lastTrippedSensor != SensorName.EASTDOWN) tryToAcquire(down,3,11,2);

            if (semaphores.contains(mid)) releaseSemaphore(mid);
            else if (lastTrippedSensor != SensorName.SOUTHLEFT) tryToAcquire(mid,4,9,1);
            break;
          case EASTLEFT:
            if (lastTrippedSensor == SensorName.WESTRIGHT) {
              waitForAcquire(left);
              tsim.setSwitch(4,9,1);
            } else releaseSemaphore(left);
            break;
          case SOUTHLEFT:
            if (lastTrippedSensor == SensorName.SOUTHRIGHT) {
              waitForAcquire(left);
              tsim.setSwitch(4,9,2);
            } else releaseSemaphore(left);
            break;
          case WESTRIGHT:
            if (lastTrippedSensor == SensorName.EASTLEFT) {
              waitForAcquire(right);
              tsim.setSwitch(15, 9, 2);
            } else releaseSemaphore(right);
            break;
          case EASTRIGHT:
            if (lastTrippedSensor == SensorName.SOUTHUP) releaseSemaphore(up);
            else if (lastTrippedSensor != SensorName.WESTUP) tryToAcquire(up,17,7,1);

            if (semaphores.contains(mid)) releaseSemaphore(mid);
            else if (lastTrippedSensor != SensorName.SOUTHRIGHT) tryToAcquire(mid,15,9,2);
            break;
          case SOUTHRIGHT:
            if (lastTrippedSensor == SensorName.SOUTHLEFT) {
              waitForAcquire(right);
              tsim.setSwitch(15,9,1);
            } else releaseSemaphore(right);
            break;
          case EASTDOWN:
            if (lastTrippedSensor == SensorName.WESTLEFT) {
              releaseSemaphore(left);
              stopAtStation(9);
            } else {
              waitForAcquire(left);
              tsim.setSwitch(3,11,1);
            }
            break;
          case SOUTHDOWN:
            if (lastTrippedSensor == SensorName.WESTLEFT) {
              releaseSemaphore(left);
              stopAtStation(11);
            } else {
              waitForAcquire(left);
              tsim.setSwitch(3, 11, 2);
            }
            break;
        }
      }
      lastTrippedSensor = sensorName;
    }

    @Override
    public void run() {
      while (!this.isInterrupted()) {
       try {
         pollSensorEvent(tsim.getSensor(id));
      } catch (InterruptedException e) {
        e.printStackTrace();
      } catch (CommandException e) {
        e.printStackTrace();
      }
      }
    }
  }
}
