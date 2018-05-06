

class Robot:
    '''Представляет робота с именем.'''
    # Переменная класса, содержащая количество роботов
    population = 0

    def __init__(self, name):
        '''Инициализация данных.'''
        self.name = name
        self.population = 1
        Robot.population += 1

        print('(Инициализация {0})'.format(self.name))


r1 = Robot("Bob")
print (r1.name)
print (r1.population)

r2 = Robot("Greg")
print (r2.name)
print (r2.population)

r3 = Robot ("Mari")
print (r3.name)
print (r3.population)
print (Robot.population)
