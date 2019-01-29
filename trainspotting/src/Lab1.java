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
  private Semaphore middown = new Semaphore(1);
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

  private Map<Point2D,SensorName> posSensor = new HashMap<>();


  public Lab1(int speed1, int speed2) {
    TSimInterface tsi = TSimInterface.getInstance();
    train1 = new Train(1,tsi,speed1);
    train2 = new Train(2,tsi,speed2);
    System.out.println(speed1 + ", " + speed2);
    try {
      train1.setSpeed(speed1);
      train2.setSpeed(speed2);

      tsi.setSwitch(4,9,0x01);
      tsi.setSwitch(15,9,0x02);
      init();
    }
    catch (CommandException e) {
      e.printStackTrace();    // or only e.getMessage() for the error
      System.exit(1);
    }

    train1.start();
    train2.start();

  }

  private void init() throws CommandException{
    //train1.setSpeed(10);
    //train2.setSpeed(10);


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

    private SensorName lastTrippedSensor = null;
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

    synchronized void waitForAcquire(Semaphore semaphore) throws InterruptedException, CommandException {
      if (!semaphore.tryAcquire()){
        setSpeed(0);
        semaphore.acquire();
        setSpeed(maxSpeed);
        semaphores.add(semaphore);
      }
    }

    /*void updateSemaphore(Semaphore semaphore) {
      if (semaphore == currentSemaphore) {
        currentSemaphore = lastSemaphore;
        lastSemaphore = null;
      } else if (lastSemaphore == semaphore) {
        lastSemaphore = null;
      } else {
        lastSemaphore = currentSemaphore;
        currentSemaphore = semaphore;
      }

    }*/

    void releaseSemaphore(Semaphore semaphore) {
      semaphore.release();
      semaphores.remove(semaphore);
    }

    void pollSensorEvent(SensorEvent sensor) throws InterruptedException, CommandException{
      SensorName sensorName = posSensor.get((new Point2D(sensor.getXpos(),sensor.getYpos())));
      System.out.println(sensorName);
      if (sensor.getStatus() == SensorEvent.ACTIVE) {
        switch (sensorName) {
          case NORTHNORTHSTATION:
            stopAtStation();
            break;
          case NORTHSOUTHSTATION:
            stopAtStation();
            break;
          case SOUTHNORTHSTATION:
            stopAtStation();
            break;
          case SOUTHSOUTHSTATION:
            stopAtStation();
            break;
          case NORTHCROSS:
            if (lastTrippedSensor == SensorName.NORTHSOUTHSTATION) {
              waitForAcquire(cross);
            } else {
              releaseSemaphore(cross);
            }
            break;
          case WESTCROSS:
            if (lastTrippedSensor == SensorName.NORTHNORTHSTATION) {
              waitForAcquire(cross);
            } else {
              releaseSemaphore(cross);
            }
            break;
          case EASTCROSS:
            if (lastTrippedSensor == SensorName.WESTUP) {
              waitForAcquire(cross);
            } else {
              releaseSemaphore(cross);
            }
            break;
          case SOUTHCROSS:
            if (lastTrippedSensor == SensorName.SOUTHUP) {
              waitForAcquire(cross);
            } else {
              releaseSemaphore(cross);
            }
            break;
          case WESTUP:
            if (lastTrippedSensor == SensorName.EASTCROSS) {
              waitForAcquire(right);
              tsim.setSwitch(17,7,0x02);
            } else {
              releaseSemaphore(right);
            }
            break;
          case EASTUP:
            if (lastTrippedSensor == SensorName.EASTRIGHT) {
              if (up.tryAcquire()) {
                semaphores.add(up);
                tsim.setSwitch(17,7,0x01);
              } else {
                tsim.setSwitch(17,7,0x02);
              }
            } else if (lastTrippedSensor == SensorName.SOUTHUP){
              releaseSemaphore(up);
            }
            break;
          case SOUTHUP:
            if ( lastTrippedSensor == SensorName.SOUTHCROSS) {
              waitForAcquire(right);
              tsim.setSwitch(17,7,0x01);
            } else {
              releaseSemaphore(right);
            }
            break;
          case WESTLEFT:
            if (lastTrippedSensor == SensorName.WESTDOWN) {
              if (middown.tryAcquire()) {
                semaphores.add(middown);
                tsim.setSwitch(4,9,0x01);
              } else {
                tsim.setSwitch(4,9,0x02);
              }
            } else if (semaphores.contains(middown)){
              releaseSemaphore(middown);
            }
            break;
          case EASTLEFT:
            if (lastTrippedSensor == SensorName.WESTRIGHT) {
              waitForAcquire(left);
              tsim.setSwitch(4,9,0x01);
            } else {
              releaseSemaphore(left);
            }
            break;
          case SOUTHLEFT:
            if (lastTrippedSensor == SensorName.SOUTHRIGHT) {
              waitForAcquire(left); // Should this be synchronized with setSwitch?
              tsim.setSwitch(4,9,0x02);
            } else {
              releaseSemaphore(left);
            }
            break;
          case WESTRIGHT:
            if (lastTrippedSensor == SensorName.EASTLEFT) {
              waitForAcquire(right);
              tsim.setSwitch(15, 9, 0x02);
            } else {
              releaseSemaphore(right);
            }
            break;
          case EASTRIGHT:
            if (lastTrippedSensor == SensorName.EASTUP) {
              if (middown.tryAcquire()) {
                semaphores.add(middown);
                tsim.setSwitch(15,9,0x02);
              } else {
                tsim.setSwitch(15,9,0x01);
              }
            } else if (semaphores.contains(middown)) {
              releaseSemaphore(middown);
            }
            break;
          case SOUTHRIGHT:
            if (lastTrippedSensor == SensorName.SOUTHLEFT) {
              waitForAcquire(right);
              tsim.setSwitch(15,9,0x01);
            } else {
              releaseSemaphore(right);
            }
            break;
          case WESTDOWN:
            if (lastTrippedSensor == SensorName.WESTLEFT) {
              if (down.tryAcquire()) {
                semaphores.add(down);
                tsim.setSwitch(3,11,0x02);
              } else {
                tsim.setSwitch(3,11,0x01);
              }
            } else if (lastTrippedSensor == SensorName.SOUTHDOWN){
              releaseSemaphore(down);
            }
            break;
          case EASTDOWN:
            if (lastTrippedSensor == SensorName.SOUTHNORTHSTATION) {
              waitForAcquire(left);
              tsim.setSwitch(3,11,0x01);
            } else {
              releaseSemaphore(left);
            }
            break;
          case SOUTHDOWN:
            if (lastTrippedSensor == SensorName.SOUTHSOUTHSTATION) {
              waitForAcquire(left);
              tsim.setSwitch(3,11,0x02);
            } else {
              releaseSemaphore(left);
            }
            break;
        }
        /*System.out.println(middown.availablePermits() + " " + semaphores.contains(middown));
        if (middown.availablePermits() > 1) {
          middown = null;
          middown.release();
        }*/
      }
      lastTrippedSensor = sensorName;

    }



    void stopAtStation() throws InterruptedException, CommandException{
      setSpeed(0);
      sleep(1000 + (20 * speed));
      direction *= (-1);
      setSpeed(maxSpeed);
    }

    @Override
    public void run() {
      while (!this.isInterrupted()) {
       try {
         pollSensorEvent(tsim.getSensor(id));
      } catch (InterruptedException e ) {
        e.printStackTrace();
      } catch (CommandException e) {
        e.printStackTrace();
      }
      }
    }
  }
}
