      SUBROUTINE DLOAD(F,KSTEP,KINC,TIME,NOEL,NPT,LAYER,KSPT, COORDS,JLTYP,SNAME)

      INCLUDE 'ABA_PARAM.INC'

      DIMENSION TIME(2), COORDS (3)
      CHARACTER*80 SNAME
c     施加公路桥涵通用设计规范中规定的55t重标准车辆移动荷载
c     user coding to define F
      real x, z, v, disp, a, b, len  ! a 轮胎接触面宽度；b 轮胎接触面高度；len 车辆行驶方向构件长度
      real, dimension(5) :: rod  ! 车轴间距：3m、1.4m、7m、1.4m / 水平轮距1.8m
     
      rod(1) = 3000  ! 车轴间距
      rod(2) = 1400  ! 车轴间距
      rod(3) = 7000  ! 车轴间距
      rod(4) = 1400  ! 车轴间距
      rod(5) = 1800  ! 车轮间距

      x = coords(1)
      z = coords(3)

      a = 600
      b = 200
      len = 20000
      v = len + rod(1) + rod(2) + rod(3) + rod(4) + b ! 前后轮外侧间距加上车辆行驶方向构件尺寸（step时间为1）
      disp = v * time(2)
c     施加的F为pressure，单位面积力，需要换算
      if ((x <= (rod(5) + a)/2 .and. x >= (rod(5) - a)/2) .or. (x <= -(rod(5) - a)/2 .and. x >= -(rod(5) + a)/2)) then
		if ((z >= disp - b) .and. (z <= disp)) then 
			F = 0.125
		elseif ((z >= disp - b - rod(1)) .and. (z <= disp - rod(1)))then
			F = 0.5
		elseif ((z >= disp - b - rod(1) - rod(2)) .and. (z <= disp - rod(1) - rod(2)))then
			F = 0.5
		elseif ((z >= disp - b - rod(1) - rod(2) - rod(3)) .and. (z <= disp - rod(1) - rod(2) - rod(3)))then
			F = 0.58
		elseif ((z >= disp - b - rod(1) - rod(2) - rod(3) - rod(4)) .and. (z <= disp - rod(1) - rod(2) - rod(3) - rod(4)))then
			F = 0.58
		else
			F = 0
		end if
      else
		F = 0
      end if
      RETURN
      END
