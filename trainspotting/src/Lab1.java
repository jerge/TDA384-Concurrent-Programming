import TSim.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Semaphore;

import static java.lang.Math.min;

public class Lab1 {

  private Train train1;
  private Train train2;

  Semaphore cross = new Semaphore(0);
  Semaphore up = new Semaphore(0);
  Semaphore left = new Semaphore(0);
  Semaphore right = new Semaphore(0);
  Semaphore down = new Semaphore(0);

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

  Map<Position,SensorName> posSensor = new HashMap<>();

  private class Position {
    int x,y;
    Position(int x, int y) {
      this.x=x;
      this.y=y;
    }

    @Override
    public boolean equals(Object obj) {
      if (obj instanceof Position) {
        Position ob = (Position) obj;
        return ob.x ==x && ob.y == y;
      }
      return super.equals(obj);
    }
  }

  public Lab1(int speed1, int speed2) {
    TSimInterface tsi = TSimInterface.getInstance();
    train1 = new Train(1,tsi);
    train2 = new Train(2,tsi);
    try {
      init(speed1, speed2);
    }
    catch (CommandException e) {
      e.printStackTrace();    // or only e.getMessage() for the error
      System.exit(1);
    }
  }

  private void init(int speed1, int speed2) throws CommandException{
    train1.setSpeed(speed1);
    train2.setSpeed(speed2);

    posSensor.put(new Position(15,3), SensorName.NORTHNORTHSTATION);
    posSensor.put(new Position(15,5), SensorName.NORTHSOUTHSTATION);

    posSensor.put(new Position(15,11), SensorName.SOUTHNORTHSTATION);
    posSensor.put(new Position(15,13), SensorName.SOUTHSOUTHSTATION);

    posSensor.put(new Position(8,6), SensorName.NORTHCROSS);
    posSensor.put(new Position(7,7), SensorName.WESTCROSS);
    posSensor.put(new Position(9,7), SensorName.EASTCROSS);
    posSensor.put(new Position(8,8), SensorName.SOUTHCROSS);

    posSensor.put(new Position(16,7), SensorName.WESTUP);
    posSensor.put(new Position(18,7), SensorName.EASTUP);
    posSensor.put(new Position(17,8), SensorName.SOUTHUP);

    posSensor.put(new Position(14,9), SensorName.WESTLEFT);
    posSensor.put(new Position(16,9), SensorName.EASTLEFT);
    posSensor.put(new Position(15,10), SensorName.SOUTHLEFT);

    posSensor.put(new Position(3,9), SensorName.WESTRIGHT);
    posSensor.put(new Position(5,9), SensorName.EASTRIGHT);
    posSensor.put(new Position(4,10), SensorName.SOUTHRIGHT);

    posSensor.put(new Position(2,11), SensorName.WESTDOWN);
    posSensor.put(new Position(4,11), SensorName.EASTDOWN);
    posSensor.put(new Position(3,12), SensorName.SOUTHDOWN);

  }

  private class Train extends Thread{
    private final int id;
    private final TSimInterface tsim;
    // 1 is starting direction, -1 is backwards direction
    private int direction = 1;
    private int speed = 0;
    private final int maxSpeed = 15;

    private SensorEvent lastTrippedSensor = null;

    Train(int id, TSimInterface tsim) {
      this.id = id;
      this.tsim = tsim;
    }

    void setSpeed(int spd) throws CommandException{
      spd = min(maxSpeed, spd);
      this.speed = spd;
      tsim.setSpeed(id, speed*direction);
    }

    void pollSensorEvent() throws InterruptedException, CommandException{
      SensorEvent sensor = tsim.getSensor(id);
      SensorName sensorName = posSensor.get((new Position(sensor.getXpos(),sensor.getYpos())));
      if (sensor.getStatus() == SensorEvent.ACTIVE
              && sensor.getTrainId() == id
              && sensor != lastTrippedSensor) {
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
            break;
          case WESTCROSS:
            break;
          case EASTCROSS:
            break;
          case SOUTHCROSS:
            break;
          case WESTUP:
            break;
          case EASTUP:
            break;
          case SOUTHUP:
            break;
          case WESTLEFT:
            break;
          case EASTLEFT:
            break;
          case SOUTHLEFT:
            break;
          case WESTRIGHT:
            break;
          case EASTRIGHT:
            break;
          case SOUTHRIGHT:
            break;
          case WESTDOWN:
            break;
          case EASTDOWN:
            break;
          case SOUTHDOWN:
            break;
        }
        lastTrippedSensor = sensor;
      }
    }



    void stopAtStation() throws InterruptedException, CommandException{
      setSpeed(0);
      direction *= (-1);
      this.wait(1000 + (20 * speed));
      setSpeed(1);
    }

    @Override
    public void run() {
      while (!this.isInterrupted()) {
       try {
         pollSensorEvent();
      } catch (InterruptedException e ) {
        e.printStackTrace();
      } catch (CommandException e) {
        e.printStackTrace();
      }
      }
    }
  }
}
