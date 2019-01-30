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

  private Semaphore cross = new Semaphore(1);
  private Semaphore up = new Semaphore(1);
  private Semaphore left = new Semaphore(1);
  private Semaphore mid = new Semaphore(1);
  private Semaphore right = new Semaphore(1);
  private Semaphore down = new Semaphore(1);

  private enum SensorName {
    // Station
    NORTHNORTHSTATION,
    NORTHSOUTHSTATION,
    SOUTHNORTHSTATION,
    SOUTHSOUTHSTATION,
    // Crossing
    NORTHCROSS,
    WESTCROSS,
    EASTCROSS,
    SOUTHCROSS,
    // Up Semaphore
    WESTUP,
    EASTUP,
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
    WESTDOWN,
    EASTDOWN,
    SOUTHDOWN
  }

  // Position to sensor map
  private Map<Point2D,SensorName> posSensor = new HashMap<>();

  public Lab1(int speed1, int speed2) {
    TSimInterface tsi = TSimInterface.getInstance();
    train1 = new Train(1,tsi,speed1);
    train2 = new Train(2,tsi,speed2);
    System.out.println(speed1 + ", " + speed2);
    try {
      train1.setSpeed(speed1);
      train2.setSpeed(speed2);

      tsi.setSwitch(4,9,1);
      tsi.setSwitch(15,9,2);
      init();
    }
    catch (CommandException e) {
      e.printStackTrace();    // or only e.getMessage() for the error
      System.exit(1);
    }

    train1.start();
    train2.start();

  }

  private void init(){
    posSensor.put(new Point2D(15,3), SensorName.NORTHNORTHSTATION);
    posSensor.put(new Point2D(15,5), SensorName.NORTHSOUTHSTATION);

    posSensor.put(new Point2D(15,11), SensorName.SOUTHNORTHSTATION);
    posSensor.put(new Point2D(15,13), SensorName.SOUTHSOUTHSTATION);

    posSensor.put(new Point2D(9,5), SensorName.NORTHCROSS);
    posSensor.put(new Point2D(6,6), SensorName.WESTCROSS);
    posSensor.put(new Point2D(11,7), SensorName.EASTCROSS);
    posSensor.put(new Point2D(10,8), SensorName.SOUTHCROSS);

    posSensor.put(new Point2D(14,7), SensorName.WESTUP);
    posSensor.put(new Point2D(19,7), SensorName.EASTUP);
    posSensor.put(new Point2D(15,8), SensorName.SOUTHUP);

    posSensor.put(new Point2D(13,9), SensorName.WESTRIGHT);
    posSensor.put(new Point2D(17,9), SensorName.EASTRIGHT);
    posSensor.put(new Point2D(13,10), SensorName.SOUTHRIGHT);

    posSensor.put(new Point2D(2,9), SensorName.WESTLEFT);
    posSensor.put(new Point2D(7,9), SensorName.EASTLEFT);
    posSensor.put(new Point2D(6,10), SensorName.SOUTHLEFT);

    posSensor.put(new Point2D(1,10), SensorName.WESTDOWN);
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

    void setSpeed(int spd) throws CommandException{
      spd = min(maxSpeed, spd);
      this.speed = spd;
      tsim.setSpeed(id, speed*direction);
    }

    void stopAtStation() throws InterruptedException, CommandException{
      setSpeed(0);
      sleep(1000 + (20 * speed));
      direction *= (-1);
      setSpeed(maxSpeed);
    }

    void waitForAcquire(Semaphore semaphore) throws InterruptedException, CommandException {
      if (!semaphore.tryAcquire()){
        setSpeed(0);
        semaphore.acquire();
        setSpeed(maxSpeed);
        semaphores.add(semaphore);
      }
    }

    void releaseSemaphore(Semaphore semaphore) {
      semaphore.release();
      semaphores.remove(semaphore);
    }

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
          // At stations stop and turn around. Due to max speed <= 15 the train will not pass the
          //  sensor and activate it multiple times
          // In some cases you have to wait in order to acquire,
          //  other times you have to switch direction
          case NORTHNORTHSTATION:
          case NORTHSOUTHSTATION:
          case SOUTHNORTHSTATION:
          case SOUTHSOUTHSTATION:
            stopAtStation();
            break;
          case NORTHCROSS:
            if (lastTrippedSensor == SensorName.NORTHSOUTHSTATION) waitForAcquire(cross);
            else releaseSemaphore(cross);
            break;
          case WESTCROSS:
            if (lastTrippedSensor == SensorName.NORTHNORTHSTATION) waitForAcquire(cross);
            else releaseSemaphore(cross);
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
          case EASTUP:
            if (lastTrippedSensor == SensorName.EASTRIGHT) tryToAcquire(up,17,7,1);
            else if (lastTrippedSensor == SensorName.SOUTHUP) releaseSemaphore(up);
            break;
          case SOUTHUP:
            if ( lastTrippedSensor == SensorName.SOUTHCROSS) {
              waitForAcquire(right);
              tsim.setSwitch(17,7,1);
            } else releaseSemaphore(right);
            break;
          case WESTLEFT:
            if (lastTrippedSensor == SensorName.WESTDOWN) tryToAcquire(mid,4,9,1);
            else if (semaphores.contains(mid)) releaseSemaphore(mid);
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
            if (lastTrippedSensor == SensorName.EASTUP) tryToAcquire(mid,15,9,2);
            else if (semaphores.contains(mid)) releaseSemaphore(mid);
            break;
          case SOUTHRIGHT:
            if (lastTrippedSensor == SensorName.SOUTHLEFT) {
              waitForAcquire(right);
              tsim.setSwitch(15,9,1);
            } else releaseSemaphore(right);
            break;
          case WESTDOWN:
            if (lastTrippedSensor == SensorName.WESTLEFT) tryToAcquire(down,3,11,2);
            else if (lastTrippedSensor == SensorName.SOUTHDOWN) releaseSemaphore(down);
            break;
          case EASTDOWN:
            if (lastTrippedSensor == SensorName.SOUTHNORTHSTATION) {
              waitForAcquire(left);
              tsim.setSwitch(3,11,1);
            } else releaseSemaphore(left);
            break;
          case SOUTHDOWN:
            if (lastTrippedSensor == SensorName.SOUTHSOUTHSTATION) {
              waitForAcquire(left);
              tsim.setSwitch(3,11,2);
            } else releaseSemaphore(left);
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
